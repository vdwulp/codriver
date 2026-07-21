#' Send the prompt list to the configured LLM
#'
#' @param context The RStudio source editor context from
#'   `rstudioapi::getSourceEditorContext()`.
#' @param action The action list from `resolve_action()`.
#' @param prompt The prompt list from `build_prompt()`.
#'
#' @importFrom ellmer chat
#'
#' @noRd
send_prompt <- function(context, action, prompt) {

  # ── Initialise chat instance ─────────────────────────────────────────────────
  cfg  <- get_config()           # presence checked in main
  args <- c(cfg$args, list(system_prompt = prompt$system))
  chat <- do.call(ellmer::chat, args)

  # ── Call ellmer chat with retry on transient errors ──────────────────────────
  # Retryable: HTTP 500/502/503/504 and APIConnectionError
  # Waits: 0.25s, 0.5s, 1s before attempts 2, 3, 4
  attempt     <- 0L
  timeouts    <- c(.25, .5, 1)
  max_retries <- length(timeouts) + 1L

  repeat {
    attempt <- attempt + 1L

    result <- tryCatch(
      chat$chat(prompt$user, echo = "none"),
      error = function(e) {
        msg <- conditionMessage(e)
        is_retryable <- grepl("HTTP 5(00|02|03|04)", msg) ||
          grepl("APIConnectionError", msg)

        if (is_retryable && attempt < max_retries) {
          wait <- timeouts[attempt]
          codriver_message("Request %d/%d failed, retrying in %.2gs...\n%s",
                           attempt, max_retries, wait, msg)
          Sys.sleep(wait)
          NULL  # signal retry
        } else {
          stop(sprintf("[codriver] Request failed: %s", msg), call. = FALSE)
        }
      }
    )

    if (!is.null(result)) break
  }

  # ── Post-processing ─ General ───────────────────────────────────────────────
  # Strip fenced code blocks if the entire response is wrapped in one
  if (grepl("^```", result)) {
    stripped <- sub("^```[a-zA-Z]*\n?", "", result)
    stripped <- sub("\n?```$", "", stripped)
    if (!grepl("^\\s*$", stripped)) {
      result <- stripped
    }
  }

  # Normalise whitespace-only responses to empty string
  if (grepl("^\\s*$", result)) {
    result <- ""
  }

  # ── Post-processing ─ Mode-specific ─────────────────────────────────────────
  if (action$mode == "complete") {
    # Empty response means the statement is already complete
    if (result == "") return(" # no suggestion")

    # Strip leading overlap with existing text if the model echoed it back
    if (!is.null(action$text) && startsWith(result, action$text)) {
      return(substr(result, nchar(action$text) + 1L, nchar(result)))
    }
  }

  # Empty response means no suggestion
  if (action$mode == "continue" && result == "") {
    return("# no suggestion")
  }

  # Empty response means no changes
  if (action$mode == "edit" && result == "") {
    return(action$text)
  }

  # Empty response means no suggestion or no changes
  if (action$mode == "comment" && result == "") {
    if (action$text[2] == "") {
      return("no suggestion")
    } else {
      return(action$text[2])
    }
  }

  return(result)
}

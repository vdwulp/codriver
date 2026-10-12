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

  # ── Set prompts in last chat memory ─────────────────────────────────────────
  set_last_chat(prompt$system, prompt$user)

  # ── Initialise chat instance ────────────────────────────────────────────────
  cfg  <- get_config()           # presence checked in main
  args <- c(cfg$args, list(system_prompt = prompt$system))
  chat <- do.call(ellmer::chat, args)

  # ── Call ellmer chat with retry on transient errors ─────────────────────────
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

  # ── Set raw response in last chat memory ────────────────────────────────────
  set_last_chat(assistant_response = result)

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

  result
}


# ── Last chat info ────────────────────────────────────────────────────────────

#' Inspect the most recent chat
#'
#' Returns the prompts and raw assistant response recorded for the most recent
#' codriver request in the current R session.
#'
#' @return A named list, or \code{NULL} if no chat has been recorded. The
#'   returned list may contain:
#'   \describe{
#'     \item{system_prompt}{The system prompt used for the request.}
#'     \item{user_prompt}{The user prompt sent to the language model.}
#'     \item{assistant_response}{The raw response returned by the language
#'       model.}
#'   }
#'
#' @export
codriver_last_chat <- function() {
  last_chat <- .codriver_env$last_chat

  if (!is.null(last_chat)) {
    class(last_chat) <- c("codriver_last_chat", "list")
  }

  last_chat
}

#' @export
print.codriver_last_chat <- function(x, ...) {
  rule_width <- max(56L, getOption("width") - 1L)
  rule <- strrep("\u2500", rule_width)
  header <- strtrim(
    paste0("\u2500\u2500 codriver last chat \u2500", rule),
    rule_width
  )

  write_wrapped <- function(value, width) {
    lines <- strsplit(value, "\n", fixed = TRUE)[[1]]

    for (line in lines) {
      starts <- seq.int(1L, max(1L, nchar(line)), by = width)
      writeLines(substring(line, starts, starts + width - 1L))
    }
  }

  print_section <- function(title, value) {
    cat("\n\u25b6\u25b6 ", toupper(title), "\n", sep = "")

    if (is.null(value)) {
      cat("  not recorded\n")
    } else {
      write_wrapped(value, rule_width)
    }
  }

  cat(header, "\n")

  print_section("system prompt", x$system_prompt)
  print_section("user prompt", x$user_prompt)
  print_section("assistant response", x$assistant_response)

  cat("\n", rule, "\n", sep = "")

  invisible(x)
}

#' Set the last chat details
#'
#' Stores prompts and responses in the package environment. Calling the function
#' without arguments clears the stored chat. Supplying a system or user prompt
#' starts a 'new chat' and removes any previous assistant response. Supplying
#' only an assistant response preserves the existing prompts.
#'
#' @param system_prompt Optional system prompt.
#' @param user_prompt Optional user prompt.
#' @param assistant_response Optional assistant response.
#'
#' @noRd
set_last_chat <- function(system_prompt = NULL,
                          user_prompt = NULL,
                          assistant_response = NULL) {
  if (missing(system_prompt) &&
        missing(user_prompt) &&
        missing(assistant_response)) {
    .codriver_env$last_chat <- NULL
    return(invisible())
  }

  if (!missing(system_prompt) || !missing(user_prompt)) {
    .codriver_env$last_chat <- list(
      system_prompt      = system_prompt,
      user_prompt        = user_prompt
    )
  }

  if (!missing(assistant_response)) {
    .codriver_env$last_chat$assistant_response <- assistant_response
  }

  invisible()
}

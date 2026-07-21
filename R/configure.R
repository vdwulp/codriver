#' Configure codriver
#'
#' Set up the AI provider that codriver uses to generate and edit R code and
#' comments. The configuration is saved locally and validated immediately by
#' making a test call to the provider.
#'
#' Pass any arguments supported by the underlying [ellmer::chat()] constructor
#' directly via `name` and `...`.
#'
#' If no keyboard shortcut is registered for codriver, the function offers to
#' register the default shortcut `Ctrl+/`. Shortcut registration is strongly
#' recommended but can be skipped or changed later via `Tools` >
#' `Modify Keyboard Shortcuts` in RStudio.
#'
#' @param name Character. Provider name passed to [ellmer::chat()], in the form
#'   `"provider"` or `"provider/model"`. Supported providers are listed on
#'   <https://ellmer.tidyverse.org/index.html#providers>.
#' @param ... Named arguments passed to the ellmer constructor, such as
#'   `model` or `base_url`.
#'
#' @returns Returns `TRUE` invisibly on successful provider configuration.
#'   Returns `FALSE` invisibly if RStudio is not available or provider
#'   validation fails.
#'
#' @seealso
#'   * `vignette("getting-started-with-codriver")` for extended information on
#'     configuring codriver
#'   * `vignette("start-using-codriver")` for an overview of modes and result
#'     presentation
#'   * [ellmer::chat()] and supported providers at
#'   <https://ellmer.tidyverse.org/index.html#providers>
#'
#' @examples
#' \dontrun{
#'   codriver_configure("openai", model = "gpt-4o")
#'   codriver_configure("openai_compatible/mistral", base_url = "http://localhost:1234/v1")
#' }
#'
#' @importFrom cli cli_bullets cli_progress_done cli_progress_message
#' @importFrom ellmer chat
#' @importFrom rlang list2 %||%
#'
#' @export
codriver_configure <- function(name, ...) {

  # Check minimal requirements
  if (!check_rstudio()) return(invisible(FALSE))

  # ── Provider settings ───────────────────────────────────────────────────────
  args <- list2(...)

  # Merge model into name if supplied separately and not already in name
  if (!is.null(args$model) && !grepl("/", name, fixed = TRUE)) {
    name <- paste0(name, "/", args$model)
    args$model <- NULL
  }

  args <- c(list(name = name), args)

  # Validate settings
  cli_progress_message("Validating provider configuration...")

  error_msg <- tryCatch({
    chat <- do.call(ellmer::chat, args)
    chat$chat("Reply only with OK.")
    NULL
  }, error = function(e) {
    msg <- conditionMessage(e)
    msg <- gsub("\033\\[[0-9;]*[A-Za-z]", "", msg, perl = TRUE)  # strip ANSI

    # Suppress openai_compatible specific message about non-codriver function
    if (startsWith(name, "openai_compatible")) {
      msg <- gsub(
        "\u2139 Use `chat_openai()` if you want to use OpenAI's official API.",
        "",
        msg,
        fixed = TRUE
      )
    }

    msg
  })

  cli_progress_done()

  if (!is.null(error_msg)) {
    cli_bullets(c("x" = "Configuration failed: {error_msg}"))
    return(invisible(FALSE))
  }

  # Save settings
  set_config(list(args = args))
  cli_bullets(c("v" = "Provider configured successfully!"))

  # ── Shortcut logic ──────────────────────────────────────────────────────────
  current_shortcut <- is_shortcut_registered()

  if (is.null(current_shortcut)) {

    # No shortcut set
    cli_bullets(c(
      " " = "",
      "i" = "A keyboard shortcut is strongly recommended for an optimal experience with codriver.",
      "*" = "We can set the default shortcut Ctrl+/ now..."
    ))

    # Try to set default shortcut, user can decline
    if (!register_shortcut("Ctrl+/")) {
      # Default shortcut declined, recommend to set via menu
      cli_bullets(c(
        "x" = "No shortcut was registered.",
        "*" = "You can assign one later via `Tools` > `Modify Keyboard Shortcuts`."
      ))
    }

  } else if (!nzchar(current_shortcut)) {

    # Empty shortcut set, recommend to set via menu
    cli_bullets(c(
      " " = "",
      "i" = "A keyboard shortcut is strongly recommended for an optimal experience with codriver.",
      "*" = "You can assign one via `Tools` > `Modify Keyboard Shortcuts`."
    ))
  }

  # Return success - provider configuration saved
  invisible(TRUE)
}

# ── Config path and I/O ───────────────────────────────────────────────────────

#' Get path of config file
#'
#' @importFrom tools R_user_dir
#'
#' @noRd
get_config_path <- function() {
  file.path(R_user_dir("codriver", "config"), "config.rds")
}

#' Read config file → named list
#'
#' @noRd
read_config <- function() {
  path <- get_config_path()
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

#' Write config file
#'
#' @noRd
write_config <- function(cfg) {
  path <- get_config_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cfg, path)
  invisible()
}


# ── Get/set config ────────────────────────────────────────────────────────────

#' Get the current configuration
#'
#' Returns the in-memory config list populated at package load. No file I/O.
#' All keys are always present; unset keys are `NULL`.
#'
#' @return Named list with configuration.
#'
#' @noRd
get_config <- function() {
  .codriver_env$config
}

#' Update and persist configuration values
#'
#' Replaces the current configuration with config. Writes to disk and updates
#' the in-memory cache in one step.
#'
#' @param config Named list of key/value pairs to set or remove.
#'
#' @noRd
set_config <- function(config) {
  stopifnot(is.list(config))
  write_config(config)
  .codriver_env$config <- config
  invisible()
}


# ── Shortcut helpers ──────────────────────────────────────────────────────────

#' Check whether the codriver shortcut is registered in addins.json
#'
#' Returns the registered shortcut string, "" if cleared, or NULL if absent.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom rstudio.prefs rstudio_config_path
#'
#' @noRd
is_shortcut_registered <- function() {
  path <- rstudio_config_path("keybindings/addins.json")
  if (!file.exists(path)) return(NULL)
  bindings <- fromJSON(path)
  bindings[["codriver::codriver"]]
}

#' Register a shortcut via rstudio.prefs
#'
#' @importFrom rlang :=
#' @importFrom rstudio.prefs use_rstudio_keyboard_shortcut
#'
#' @noRd
register_shortcut <- function(shortcut) {
  suppressWarnings(
    use_rstudio_keyboard_shortcut(
      !!shortcut := "codriver::codriver",
      .backup = FALSE
    )
  )
  nzchar(is_shortcut_registered() %||% "")
}

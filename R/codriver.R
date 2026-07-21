#' Context-aware AI assistant for RStudio
#'
#' Inspects the current editor context — cursor position and any selection —
#' and automatically chooses and executes one of five modes: complete, continue,
#' generate, edit, or comment. Results are presented as ghost text or an edit
#' suggestion directly in the source editor.
#'
#' Codriver is intended to be invoked via a keyboard shortcut **Ctrl+/**
#' (default) or the RStudio **Addins** menu, not called directly. Use
#' [codriver_configure()] to set up a provider and register the recommended
#' shortcut before first use.
#'
#' @returns Returns `TRUE` invisibly on success. Returns `FALSE` invisibly if
#'   RStudio is not available, no configuration is found, or the mode cannot be
#'   resolved.
#'
#' @seealso
#'   * [codriver_configure()] to set up the LLM provider and recommended
#'     keyboard shortcut
#'   * `vignette("getting-started-with-codriver")` for extended information on
#'     configuring codriver
#'   * `vignette("start-using-codriver")` for an overview of modes and result
#'     presentation
#'
#' @export
codriver <- function() {
  t_total_1 <- Sys.time()

  # Check minimal requirements
  if (!check_rstudio()) return(invisible(FALSE))

  # Check configuration
  if (is.null(get_config())) {
    codriver_message("No configuration found, run `codriver::codriver_configure()` first.")
    return(invisible(FALSE))
  }

  # Get editor context
  context <- rstudioapi::getSourceEditorContext()

  # Resolve action
  action <- resolve_action(context)

  switch(
    action$mode,
    "complete" = codriver_message(
      "Complete at %s:%s",
      action$position[["row"]],
      action$position[["column"]]
    ),
    "continue" = codriver_message(
      "Continue at %s:%s",
      action$position[["row"]],
      action$position[["column"]]
    ),
    "generate" = codriver_message(
      "Generate at %s:%s with instruction '%s'",
      action$position[["row"]],
      action$position[["column"]],
      action$text
    ),
    "edit" = codriver_message(
        paste(
          "Edit from %s:%s to %s:%s with original code",
          if (grepl("\n", action$text, fixed = TRUE)) "```\n%s\n```" else "```%s```"
        ),
        action$range$start[["row"]],
        action$range$start[["column"]],
        action$range$end[["row"]],
        action$range$end[["column"]],
        action$text
      ),
    "comment" = codriver_message(
      paste0(
        "Inline comment at %s:%s",
        if (action$text[2] == "") "" else paste0(" with original text '", action$text[2], "'")
      ),
      action$position[["row"]],
      action$position[["column"]]
    ),
    {
      codriver_message("Unable to resolve mode")
      return(invisible(FALSE))
    }
  )

  # Build prompt
  prompt <- build_prompt(context, action)

  # Send prompt
  t_llm_1 <- Sys.time()
  result <- send_prompt(context, action, prompt)
  t_llm_2 <- Sys.time()

  # Show result
  show_result(context, action, prompt, result)

  t_total_2 <- Sys.time()

  # Print timing
  t_llm   <- as.numeric(difftime(t_llm_2, t_llm_1, units = "secs"))
  t_total <- as.numeric(difftime(t_total_2, t_total_1, units = "secs"))

  codriver_message("Completed in %.1fs, %.0f%% generation",
                   t_total,
                   t_llm / t_total * 100)

  invisible(TRUE)
}

#' Print a codriver message
#'
#' @noRd
codriver_message <- function(msg, ...) {
  msg <- if (length(list(...)) == 0L) msg else sprintf(msg, ...)
  message("[codriver] ", msg)
}

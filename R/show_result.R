#' Show the LLM result in the RStudio editor
#'
#' @param context The RStudio source editor context from
#'   `rstudioapi::getSourceEditorContext()`.
#' @param action The action list from `resolve_action()`.
#' @param prompt The prompt list from `build_prompt()`.
#' @param result The LLM output string from `send_prompt()`.
#'
#' @importFrom rstudioapi document_position document_range
#' @importFrom rstudioapi getSourceEditorContext insertText setCursorPosition
#' @importFrom rstudioapi showEditSuggestion
#'
#' @noRd
show_result <- function(context, action, prompt, result) {

  id <- context$id

  # ── Cosmetic changes ────────────────────────────────────────────────────────

  # Prepend space to inline comment if position is not directly after a space
  if (action$mode == "comment" &&
      substr(context$contents[action$position[["row"]]],
             action$position[["column"]] - 1,
             action$position[["column"]] - 1) != " ") {
    result <- paste0(" ", result)
  }

  # Prepend space to complete mode result if not yet there and token justifies
  if (action$mode == "complete") {
    tokens_before <- tokens_result <- c("%>%", "|>")

    before <- substr(context$contents[action$position[["row"]]],
                     1,
                     action$position[["column"]] - 1)

    if ((any(endsWith(before, tokens_before)) && !startsWith(result, " ")) ||
        (!endsWith(before, " ") && any(startsWith(result, tokens_result)))) {
      result <- paste0(" ", result)
    }
  }

  # ── Result presentation ─────────────────────────────────────────────────────

  if (action$mode %in% c("complete", "continue", "generate") ||
      (action$mode == "comment" && action$text[2] == "")) {

    # UI fixes for generate mode
    if (action$mode == "generate") {
      if (action$position[["row"]] > length(context$contents)) {
        # Insertion point past end of file — prepend newline to result and reposition
        result <- paste0("\n", result)
        action$position <- rstudioapi::document_position(
          length(context$contents),
          nchar(context$contents[length(context$contents)]) + 1L
        )
      } else if (context$contents[action$position[["row"]]] != "") {
        # Insertion line is non-empty — insert newline before it
        rstudioapi::insertText(
          location = rstudioapi::document_position(action$position[["row"]], 1L),
          id = id,
          text = "\n"
        )
        rstudioapi::getSourceEditorContext() # sync with RStudio
      }
    }

    # Show edit suggestion - showing as ghost text
    rstudioapi::setCursorPosition(action$position, id = id)
    rstudioapi::getSourceEditorContext() # sync with RStudio
    rstudioapi::showEditSuggestion(
      rstudioapi::document_range(action$position, action$position),
      result, id = id
    )

  } else if (action$mode == "edit" ||
             (action$mode == "comment" && action$text[2] != "")) {

    # Show edit suggestion - showing as green/red highlighting
    rstudioapi::setCursorPosition(action$range$start, id = id)
    rstudioapi::getSourceEditorContext() # sync with RStudio
    rstudioapi::showEditSuggestion(action$range, result, id = id)

  }

  invisible(TRUE)
}

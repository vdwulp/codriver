#' Build system and user prompts from a resolved action and editor context
#'
#' @param context The RStudio source editor context from
#'   `rstudioapi::getSourceEditorContext()`.
#' @param action The action list from `resolve_action()`.
#'
#' @importFrom utils head tail
#'
#' @noRd
build_prompt <- function(context, action) {
  lines <- context$contents
  n     <- length(lines)

  # ── Formatting ──────────────────────────────────────────────────────────────

  block <- function(label, content) {
    if (is.null(content) || !nzchar(trimws(content))) return(NULL)
    c(sprintf("%s: \u25c0START\u25b6%s\u25c0END\u25b6", label, content), "")
  }

  # ── Context extractors ──────────────────────────────────────────────────────

  # Extract content within a range
  lines_in_range <- function(range, n_lines = NULL, from_end = FALSE) {
    start_row <- range$start[["row"]]
    start_col <- range$start[["column"]]
    end_row   <- range$end[["row"]]
    end_col   <- range$end[["column"]]

    result <- lines[start_row:end_row]

    if (length(result) > 0L) {
      result[length(result)] <- substr(result[length(result)], 1L, end_col - 1L)
      result[1L]             <- substr(result[1L], start_col, nchar(result[1L]))
    }

    if (!is.null(n_lines) && length(result) > n_lines) {
      result <- if (from_end) tail(result, n_lines) else head(result, n_lines)
    }

    result
  }

  # Extract preamble lines: library(), require(), source(), options() calls from
  # start of file up to position
  preamble <- function(end = action$position) {
    if (end[["row"]] > n)
      end <- rstudioapi::document_position(n, nchar(lines[n]) + 1L)

    range        <- rstudioapi::document_range(rstudioapi::document_position(1L, 1L), end)
    scoped_lines <- lines_in_range(range)
    matched      <- scoped_lines[grepl("^\\s*(library|require|source|options)\\s*\\(", scoped_lines)]

    if (length(matched) == 0L) return("No library calls above cursor.")
    paste(matched, collapse = "\n")
  }

  # Lines of context before `end` position
  before <- function(n_lines, end = action$position) {
    if (end[["row"]] > n)
      end <- rstudioapi::document_position(n, nchar(lines[n]) + 1L)

    paste(
      lines_in_range(
        rstudioapi::document_range(rstudioapi::document_position(1L, 1L), end),
        n_lines = n_lines,
        from_end = TRUE
      ),
      collapse = "\n"
    )
  }

  # Lines of context after `start` position
  after <- function(n_lines, start = action$position) {
    if (start[["row"]] > n) return(NULL)

    paste(
      lines_in_range(
        rstudioapi::document_range(
          start,
          rstudioapi::document_position(n, nchar(lines[n]) + 1L)
        ),
        n_lines = n_lines
      ),
      collapse = "\n"
    )
  }

  # ── Per-mode assembly ───────────────────────────────────────────────────────

  switch(
    action$mode,

    complete = {
      # Use action$range$start to place cursor marker where token used to start
      code_before <- before(30L, end = action$range$start)
      code_after  <- after(10L)
      list(
        system = paste(
          "You are a coding assistant embedded in RStudio.",
          "Complete the last line of the R code at the position marked \u25c0CURSOR\u25b6.",
          "Return only the R code to append at the cursor - nothing else."
        ),
        user = paste(c(
          block("Preamble",      preamble()),
          block("Workspace",     build_prompt_workspace(c(code_before, code_after))),
          block("Code window",   paste0(code_before, "\u25c0CURSOR\u25b6")),
          if (nzchar(action$text))
            c(sprintf("RULE: The code must start with %s", action$text), ""),
          block("Context after", code_after)
        ), collapse = "\n")
      )
    },

    continue = {
      code_before <- before(30L)
      code_after  <- after(20L)
      list(
        system = paste(
          "You are a coding assistant embedded in RStudio.",
          "Write R code for a new line or block at the position marked \u25c0CURSOR\u25b6.",
          "Return only valid R code to insert - nothing else."
        ),
        user = paste(c(
          block("Preamble",      preamble()),
          block("Workspace",     build_prompt_workspace(c(code_before, code_after))),
          block("Code window",   paste0(code_before, "\u25c0CURSOR\u25b6")),
          block("Context after", code_after)
        ), collapse = "\n")
      )
    },

    edit = {
      code_before <- before(30L)
      code_after  <- after(10L, start = action$range$end)
      list(
        system = paste(
          "You are a coding assistant embedded in RStudio.",
          "Improve or fix the selected R code, preserving semantics where reasonable.",
          "Return only the revised R code - nothing else.",
          "If no changes are advised, return an empty string."
        ),
        user = paste(c(
          block("Preamble",       preamble()),
          block("Workspace",      build_prompt_workspace(c(code_before, action$text, code_after))),
          block("Context before", code_before),
          block("Target",         action$text),
          block("Context after",  code_after)
        ), collapse = "\n")
      )
    },

    generate = {
      code_before <- before(30L)
      code_after  <- after(10L)
      list(
        system = paste(
          "You are a coding assistant embedded in RStudio.",
          "Write R code that fulfills the user's instruction. The surrounding context is secondary.",
          "Your code will be inserted on a new line after 'Context before'.",
          "Return only the R code to insert - nothing else."
        ),
        user = paste(c(
          block("Preamble",       preamble()),
          block("Workspace",      build_prompt_workspace(c(code_before, code_after))),
          block("Context before", code_before),
          block("Instruction",    action$text),
          block("Context after",  code_after)
        ), collapse = "\n")
      )
    },

    comment = list(
      system = paste(
        "You are a coding assistant embedded in RStudio.",
        "Write a short inline comment for the R code provided.",
        "Return 1 to 5 words, no leading '#' - nothing else."
      ),
      user = paste(c(
        block("Context before",  before(10L)),
        block("Code to comment", action$text[1]),
        if (nzchar(action$text[2]))
          block("Suggestion",    action$text[2]),
        block("Context after",   after(10L))
      ), collapse = "\n")
    ),

    stop(sprintf("Unknown mode: %s", action$mode))
  )
}

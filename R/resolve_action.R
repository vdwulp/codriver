#' Returns a named list with field 'mode' and mode-dependent fields:
#'
#'   mode      field       description
#'   =======   =========   ===================================================
#'   edit      text        selected code to be edited
#'             range       range of the selection
#'             position    start of the selection (= range$start)
#'   generate  text        instruction text (comment markers stripped)
#'             range       range of the instruction comment(s)
#'             position    insertion point (row after instruction comment)
#'   complete  text        incomplete pipe token ("%>" or "%", "" if none)
#'             range       span of the token; if text == "" a point range at
#'                         the insertion position
#'             position    insertion point (cursor or start of selection)
#'   continue  position    insertion point (current empty line)
#'   comment   text        vector with `[1]` code to comment, `[2]` current
#'                         inline comment (without comment marker)
#'             range       range of inline comment (including marker position)
#'             position    start of inline comment (comment marker position)
#'
#' range    — for edit: the span to replace; for generate: the instruction
#'            source span
#' position — the row/column at which generated, completed, or continued text is
#'            inserted
#' range end column is exclusive (RStudio convention: col n means up to and
#' including col n-1)
#'
#' @param context The RStudio source editor context from
#'   `rstudioapi::getSourceEditorContext()`.
#'
#' @noRd
resolve_action <- function(context) {

  sel   <- rstudioapi::primary_selection(context)
  text  <- sel$text
  range <- sel$range
  start <- range$start
  end   <- range$end

  # Scan contents for comment markers not in strings up to end row of selection
  # Note: avoid direct use of comment_cols, use helpers is_comment_row(row) and
  #       get_comment_column(row) instead
  comment_cols <- scan_lines(context$contents, end[["row"]])

  # ── Helpers ────────────────────────────────────────────────────────────────

  # Checks if the row is a comment line (# is first non-whitespace)
  is_comment_row <- function(row) {
    col <- get_comment_column(row)
    !is.null(col) && is_whitespace(substr(context$contents[row], 1L, col - 1L))
  }

  # Gets column of comment marker on a row (NULL if no comment marker)
  get_comment_column <- function(row) {
    result <- comment_cols[[row]]
    if (result < 0L) return(NULL)
    result
  }

  # Strips comment markers and leading/trailing whitespace from 'lines'
  strip_comment_markers <- function(lines) {
    trimws(gsub("^\\s*#\\s*", "", lines))
  }

  # Checks if a string is whitespace only
  is_whitespace <- function(s) grepl("^\\s*$", s)

  # Returns an rstudioapi-range spanning from/to just one position
  point_range <- function(pos) rstudioapi::document_range(pos, pos)

  # Returns an rstudioapi-range spanning the full 'line' on the 'row'
  line_range <- function(row, line) {
    rstudioapi::document_range(
      rstudioapi::document_position(row, 1L),
      rstudioapi::document_position(row, nchar(line) + 1L)
    )
  }

  # Build complete-mode action list, handle specific trailing tokens
  complete_action <- function(row, col) {
    line      <- context$contents[row]
    left_trim <- sub("\\s*$", "", substr(line, 1L, col - 1L))
    token     <- ""

    for (tok in c("%>", "%")) {
      if (endsWith(left_trim, tok)) {
        if (tok == "%" && endsWith(left_trim, ">%")) next  # % is not a partial pipe
        token <- tok
        break
      }
    }

    insert_col <- nchar(left_trim) + 1L
    insert_pos <- rstudioapi::document_position(row, insert_col)

    list(
      mode     = "complete",
      text     = token,
      range    = rstudioapi::document_range(
        rstudioapi::document_position(row, insert_col - nchar(token)),
        insert_pos
      ),
      position = insert_pos
    )
  }

  # Build comment-mode action list, split line into code and inline comment
  comment_action <- function(row, comment_column) {
    line        <- context$contents[row]
    comment_col <- comment_column +
      if (substr(line, comment_column + 1L, comment_column + 1L) == " ") 2L else 1L
    pos <- rstudioapi::document_position(row, comment_col)

    list(
      mode     = "comment",
      text     = c(trimws(substr(line, 1L, comment_column - 1L)),
                   strip_comment_markers(substr(line, comment_column, nchar(line)))),
      range    = rstudioapi::document_range(pos, rstudioapi::document_position(row, nchar(line) + 1L)),
      position = pos
    )
  }

  # ── No selection ───────────────────────────────────────────────────────────

  # Decide based on cursor line and position
  if (nchar(text) == 0L) {

    cursor_row  <- start[["row"]]
    cursor_line <- context$contents[cursor_row]

    ## Cursor on a comment line -> generate from that comment
    if (is_comment_row(cursor_row)) {
      return(list(
        mode     = "generate",
        text     = strip_comment_markers(cursor_line),
        range    = line_range(cursor_row, cursor_line),
        position = rstudioapi::document_position(cursor_row + 1L, 1L)
      ))
    }

    ## Cursor on an empty line
    if (is_whitespace(cursor_line)) {

      prev_row  <- cursor_row - 1L
      prev_line <- if (cursor_row > 1L) context$contents[prev_row] else ""

      # Cursor on an empty line with a comment directly above -> generate from that comment
      if (cursor_row > 1L && is_comment_row(prev_row)) {
        return(list(
          mode     = "generate",
          text     = strip_comment_markers(prev_line),
          range    = line_range(prev_row, prev_line),
          position = rstudioapi::document_position(cursor_row, 1L)
        ))
      }

      # Cursor on an empty line with *no* comment directly above -> continue
      return(list(
        mode     = "continue",
        position = rstudioapi::document_position(cursor_row, 1L)
      ))

    }

    ## Cursor on a code line

    # Cursor on a code line and on or preceded by '#' -> comment
    comment_column <- get_comment_column(cursor_row)
    if (!is.null(comment_column) && start[["column"]] >= comment_column) {
      return(comment_action(cursor_row, comment_column))
    }

    # Cursor on a code line with whitespace after -> complete
    trailing <- substr(cursor_line, start[["column"]], nchar(cursor_line))
    if (is_whitespace(trailing)) {
      return(complete_action(cursor_row, start[["column"]]))
    }

    # Cursor on a code line with code after -> edit
    edit_range <- line_range(cursor_row, cursor_line)
    return(list(
      mode     = "edit",
      text     = cursor_line,
      range    = edit_range,
      position = edit_range$start
    ))

  }

  # ── Single-line selection ──────────────────────────────────────────────────

  if (start[["row"]] == end[["row"]]) {
    full_line <- context$contents[start[["row"]]]

    ## Whitespace only selection
    if (is_whitespace(text)) {

      # Whitespace selection on a comment line -> generate from that comment
      if (is_comment_row(start[["row"]])) {
        return(list(
          mode     = "generate",
          text     = strip_comment_markers(full_line),
          range    = line_range(start[["row"]], full_line),
          position = rstudioapi::document_position(end[["row"]] + 1L, 1L)
        ))
      }

      # Whitespace selection on an empty line
      if (is_whitespace(full_line)) {

        prev_row  <- start[["row"]] - 1L
        prev_line <- if (start[["row"]] > 1L) context$contents[prev_row] else ""

        # Whitespace selection on an empty line with a comment directly above -> generate from that comment
        if (start[["row"]] > 1L && is_comment_row(prev_row)) {
          return(list(
            mode     = "generate",
            text     = strip_comment_markers(prev_line),
            range    = line_range(prev_row, prev_line),
            position = rstudioapi::document_position(start[["row"]], 1L)
          ))
        }

        # Whitespace selection on an empty line with *no* comment directly above -> continue
        return(list(
          mode     = "continue",
          position = rstudioapi::document_position(start[["row"]], 1L)
        ))

      }

      # Whitespace selection on code line -> complete
      return(complete_action(start[["row"]], start[["column"]]))
    }

    ## Non-whitespace selection

    # Non-whitespace selection on a comment line -> generate from that comment
    if (is_comment_row(start[["row"]])) {
      return(list(
        mode     = "generate",
        text     = strip_comment_markers(text),
        range    = range,
        position = rstudioapi::document_position(end[["row"]] + 1L, 1L)
      ))
    }

    # Non-whitespace selection on a code line and starts with or is preceded by '#' -> comment
    comment_column <- get_comment_column(start[["row"]])
    if (!is.null(comment_column) && start[["column"]] >= comment_column) {
      return(comment_action(start[["row"]], comment_column))
    }

    # Non-whitespace selection on a code line *not* preceded by '#' -> edit
    return(list(
      mode     = "edit",
      text     = text,
      range    = range,
      position = range$start
    ))

  }

  # ── Multi-line selection ───────────────────────────────────────────────────

  # Helper: extract the selected portion of a line in multi-line selection
  selected_portion <- function(row) {
    line <- context$contents[row]
    if (row == start[["row"]]) {
      substr(line, start[["column"]], nchar(line))
    } else if (row == end[["row"]]) {
      substr(line, 1L, end[["column"]] - 1L)
    } else {
      line
    }
  }

  ## Phase 1: Scan selection bottom-to-top for non-whitespace content
  anchor_row  <- NA
  anchor_type <- NA

  # First non-whitespace row from bottom decides comment or code
  for (row in seq(end[["row"]], start[["row"]], by = -1L)) {
    portion <- selected_portion(row)
    if (!is_whitespace(portion)) {
      anchor_row  <- row
      anchor_type <- if (is_comment_row(row)) "comment" else "code"
      break
    }
  }

  # Selection has non-whitespace
  if (!is.na(anchor_row)) {

    # Anchor is a comment line -> generate from contiguous comment block
    if (anchor_type == "comment") {

      block_start_row <- anchor_row
      if (anchor_row > start[["row"]]) {
        for (row in seq(anchor_row - 1L, start[["row"]], by = -1L)) {
          portion <- selected_portion(row)
          if (is_whitespace(portion) || !is_comment_row(row)) break
          block_start_row <- row
        }
      }

      block_rows  <- seq(block_start_row, anchor_row)
      block_lines <- strip_comment_markers(sapply(block_rows, selected_portion))
      instruction <- paste(block_lines, collapse = "\n")

      anchor_line     <- context$contents[anchor_row]
      range_start_col <- if (block_start_row == start[["row"]]) start[["column"]] else 1L

      return(list(
        mode     = "generate",
        text     = instruction,
        range    = rstudioapi::document_range(
          rstudioapi::document_position(block_start_row, range_start_col),
          rstudioapi::document_position(anchor_row, nchar(anchor_line) + 1L)
        ),
        position = rstudioapi::document_position(anchor_row + 1L, 1L)
      ))

    } else {

      # Anchor is code line

      # Selection starts on a code line and starts with or is preceded by '#' -> comment
      if (anchor_row == start[["row"]]) {
        anchor_line    <- context$contents[anchor_row]
        comment_column <- get_comment_column(anchor_row)
        if (!is.null(comment_column) && start[["column"]] >= comment_column) {
          return(comment_action(anchor_row, comment_column))
        }
      }

      # Anchor is code line -> edit whole selection
      edit_text  <- text
      edit_range <- range

      # Selection ends at column 1 of end row -> trim trailing newline
      if (end[["column"]] == 1L) {
        edit_text       <- sub("\n$", "", text)
        actual_end_row  <- end[["row"]] - 1L
        actual_end_line <- context$contents[actual_end_row]
        edit_range <- rstudioapi::document_range(
          start,
          rstudioapi::document_position(actual_end_row, nchar(actual_end_line) + 1L)
        )
      }

      return(list(
        mode     = "edit",
        text     = edit_text,
        range    = edit_range,
        position = edit_range$start
      ))

    }

  }

  ## Phase 2: Selection is whitespace only - start line decides
  start_line <- context$contents[start[["row"]]]

  # Start line is a comment line -> generate from that comment
  if (is_comment_row(start[["row"]])) {
    return(list(
      mode     = "generate",
      text     = strip_comment_markers(start_line),
      range    = line_range(start[["row"]], start_line),
      position = rstudioapi::document_position(start[["row"]] + 1L, 1L)
    ))
  }

  # Start line is a code line
  if (!is_whitespace(start_line)) {

    # Start line is a code line and is preceded by '#' -> comment
    comment_column <- get_comment_column(start[["row"]])
    if (!is.null(comment_column) && start[["column"]] >= comment_column) {
      return(comment_action(start[["row"]], comment_column))
    }

    # Start line is a code line and is *not* preceded by '#' -> complete
    return(complete_action(start[["row"]], start[["column"]]))
  }

  # Start line is an empty line -> continue
  return(list(
    mode     = "continue",
    position = rstudioapi::document_position(start[["row"]], 1L)
  ))

}

#' Scan contents for comment markers not in strings
#'
#' Scans from line 1 up to and including end_row, tracking string/comment state
#' across lines. Handles single/double quote strings, escaped characters and
#' raw string definitions.
#'
#' Returns a list of length end_row, one integer entry per line:
#' \itemize{
#'   \item \code{> 0} — column of the first # not in a string
#'   \item \code{-1} — no comment on this line
#' }
#'
#' State machine approach focussed on tracking string state and finding the first
#' comment marker on each line. Keeps a running state across lines during the
#' scan (not stored, discarded after scan):
#'   state      0 = code, 1 = regular/backtick string, 2 = raw string
#'   quote      opening quote char when state == 1
#'   raw_close  closing pattern string when state == 2, e.g. `)--"`
#'   esc        whether previous char was a backslash (state 1 only).
#'              A trailing backslash at EOL does not carry over.
#'
#' @noRd
scan_lines <- function(lines, end_row) {

  state     <- 0L
  quote     <- ""
  raw_close <- ""
  esc       <- FALSE

  result <- vector("list", end_row)

  for (r in seq_len(end_row)) {

    chars <- strsplit(lines[[r]], "", fixed = TRUE)[[1L]]
    n     <- length(chars)

    comment_col <- -1L
    i           <- 1L

    while (i <= n) {

      ch <- chars[i]

      # ── Normal code ──────────────────────────────────────────────────────
      if (state == 0L) {

        if (ch == "#") {

          comment_col <- i
          break                              # rest of line is comment

        } else if (ch == '"' || ch == "'" || ch == "`") {

          state <- 1L
          quote <- ch
          esc   <- FALSE

        } else if ((ch == "r" || ch == "R") && i + 1L <= n &&
                   (chars[i + 1L] == '"' || chars[i + 1L] == "'")) {
          # Peek ahead to possible raw string, format r"<padding><bracket>
          # - R upper/lower case
          # - Single/double quotes,
          # - Padding is zero to multiple '-' characters (exclusive)
          # - Valid brackets are (, [ and { per R-documentation and RStudio
          #   behavior (in practice R supports | too)
          q       <- chars[i + 1L]
          j       <- i + 2L
          padding <- ""
          pad_ch  <- if (j <= n) chars[j] else ""

          if (pad_ch == "-") {
            padding <- "-"
            j       <- j + 1L
            while (j <= n && chars[j] == "-") {
              padding <- paste0(padding, "-")
              j       <- j + 1L
            }
          }

          bracket <- if (j <= n) chars[j] else ""

          if (bracket %in% c("(", "[", "{")) {
            close_bracket <- c("(" = ")", "[" = "]", "{" = "}")[bracket]
            raw_close     <- paste0(close_bracket, padding, q)
            state         <- 2L
            i             <- j              # consumed r"<padding><bracket>
          }
          # else r/R is just an identifier, no state change
        }

        # ── Regular / backtick string ────────────────────────────────────────
      } else if (state == 1L) {

        if (esc) {
          esc <- FALSE
        } else if (ch == "\\") {
          esc <- TRUE
        } else if (ch == quote) {
          state <- 0L
          quote <- ""
        }

        # ── Raw string ───────────────────────────────────────────────────────
      } else if (state == 2L) {

        close_len <- nchar(raw_close)
        if (i + close_len - 1L <= n) {
          candidate <- paste(chars[i:(i + close_len - 1L)], collapse = "")
          if (candidate == raw_close) {
            state <- 0L
            i     <- i + close_len - 1L    # incremented below
          }
        }

      }

      i <- i + 1L
    }

    result[[r]] <- comment_col
    esc         <- FALSE       # reset escape at EOL, does not carry over
  }

  result
}

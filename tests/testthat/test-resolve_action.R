# ── Helpers ───────────────────────────────────────────────────────────────────

make_context <- function(contents, row, col) {
  pos   <- rstudioapi::document_position(row, col)
  range <- rstudioapi::document_range(pos, pos)
  list(
    id        = "test",
    contents  = contents,
    selection = list(list(text = "", range = range))
  )
}

make_selection_context <- function(contents, start_row, start_col, end_row, end_col) {
  start <- rstudioapi::document_position(start_row, start_col)
  end   <- rstudioapi::document_position(end_row, end_col)
  range <- rstudioapi::document_range(start, end)
  text  <- {
    rows <- contents[start_row:end_row]
    if (start_row == end_row) {
      substr(rows[1L], start_col, end_col - 1L)
    } else {
      rows[1L]           <- substr(rows[1L], start_col, nchar(rows[1L]))
      rows[length(rows)] <- substr(rows[length(rows)], 1L, end_col - 1L)
      paste(rows, collapse = "\n")
    }
  }
  list(
    id        = "test",
    contents  = contents,
    selection = list(list(text = text, range = range))
  )
}

ra <- function(context) {
  local_mocked_bindings(
    primary_selection = function(ctx) ctx$selection[[1L]],
    .package = "rstudioapi"
  )
  resolve_action(context)
}

# Returns the comment column for a single row
scan_col <- function(lines, row) scan_lines(lines, row)[[row]]


# ── scan_lines: basic comment detection ──────────────────────────────────────

test_that("scan_lines - detects comment at start of line", {
  expect_equal(scan_col(c("# comment"), 1L), 1L)
})

test_that("scan_lines - detects comment after code", {
  expect_equal(scan_col(c("x <- 1 # comment"), 1L), 8L)
})

test_that("scan_lines - returns -1 when no comment", {
  expect_equal(scan_col(c("x <- 1"), 1L), -1L)
})

test_that("scan_lines - returns -1 for empty line", {
  expect_equal(scan_col(c(""), 1L), -1L)
})

test_that("scan_lines - detects comment with leading whitespace", {
  expect_equal(scan_col(c("  # comment"), 1L), 3L)
})


# ── scan_lines: comment inside strings ignored ────────────────────────────────

test_that("scan_lines - ignores # inside double-quoted string", {
  expect_equal(scan_col(c('x <- "# not a comment"'), 1L), -1L)
})

test_that("scan_lines - ignores # inside single-quoted string", {
  expect_equal(scan_col(c("x <- '# not a comment'"), 1L), -1L)
})

test_that("scan_lines - ignores # inside backtick string", {
  expect_equal(scan_col(c("x <- `# not a comment`"), 1L), -1L)
})

test_that("scan_lines - detects comment after closed string", {
  expect_equal(scan_col(c('x <- "str" # comment'), 1L), 12L)
})

test_that("scan_lines - ignores escaped quote inside string", {
  expect_equal(scan_col(c('x <- "he said \\"hi\\"" # comment'), 1L), 23L)
})


# ── scan_lines: raw strings ───────────────────────────────────────────────────

test_that("scan_lines - ignores # inside raw string r\"(...)\"", {
  expect_equal(scan_col(c('x <- r"(# not a comment)"'), 1L), -1L)
})

test_that("scan_lines - ignores # inside raw string with padding r\"-(...)- \"", {
  expect_equal(scan_col(c('x <- r"-(# not a comment)-"'), 1L), -1L)
})

test_that("scan_lines - ignores # inside raw string with multi-dash padding", {
  expect_equal(scan_col(c('x <- r"---(# not a comment)---"'), 1L), -1L)
})

test_that("scan_lines - detects comment after closed raw string", {
  expect_equal(scan_col(c('x <- r"(text)" # comment'), 1L), 16L)
})

test_that("scan_lines - ignores # inside raw string with square brackets", {
  expect_equal(scan_col(c('x <- r"[# not a comment]"'), 1L), -1L)
})

test_that("scan_lines - ignores # inside raw string with curly brackets", {
  expect_equal(scan_col(c('x <- r"{# not a comment}"'), 1L), -1L)
})

test_that("scan_lines - R (uppercase) raw string is recognised", {
  expect_equal(scan_col(c('x <- R"(# not a comment)"'), 1L), -1L)
})

test_that("scan_lines - r followed by non-quote is treated as identifier", {
  expect_equal(scan_col(c("result # comment"), 1L), 8L)
})


# ── scan_lines: multi-line state ──────────────────────────────────────────────

test_that("scan_lines - string spanning two lines: # on second line not a comment", {
  lines <- c('x <- "line one', '# still in string"')
  expect_equal(scan_lines(lines, 2L)[[2L]], -1L)
})

test_that("scan_lines - string closed on second line: # after close is a comment", {
  lines <- c('x <- "line one', 'end" # comment')
  expect_equal(scan_lines(lines, 2L)[[2L]], 6L)
})

test_that("scan_lines - escape at EOL does not carry over to next line", {
  lines  <- c('x <- "abc\\', '# new line"')
  result <- scan_lines(lines, 2L)
  expect_equal(result[[2L]], -1L)   # still inside string (no closing quote before #)
})


# ── scan_lines: result structure ──────────────────────────────────────────────

test_that("scan_lines - returns list of length end_row", {
  lines  <- c("x <- 1", "# comment", "y <- 2")
  result <- scan_lines(lines, 3L)
  expect_type(result, "list")
  expect_length(result, 3L)
})

test_that("scan_lines - each element is a single integer", {
  lines  <- c("x <- 1", "# comment")
  result <- scan_lines(lines, 2L)
  expect_true(all(sapply(result, function(x) is.numeric(x) && length(x) == 1L)))
})

test_that("scan_lines - scans only up to end_row", {
  lines  <- c("x <- 1", "# comment", "y <- 2")
  result <- scan_lines(lines, 2L)
  expect_length(result, 2L)
})


# ── resolve_action: no selection — comment line ───────────────────────────────

test_that("resolve_action - cursor on comment line -> generate", {
  ctx    <- make_context(c("# make a plot"), 1L, 1L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "make a plot")
  expect_equal(result$position[["row"]], 2L)
})

test_that("resolve_action - cursor on comment line strips marker and whitespace", {
  ctx    <- make_context(c("  # do something"), 1L, 3L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "do something")
})


# ── resolve_action: no selection — empty line ─────────────────────────────────

test_that("resolve_action - cursor on empty line with comment above -> generate", {
  ctx    <- make_context(c("# instruction", ""), 2L, 1L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "instruction")
  expect_equal(result$position[["row"]], 2L)
})

test_that("resolve_action - cursor on empty line with no comment above -> continue", {
  ctx    <- make_context(c("x <- 1", ""), 2L, 1L)
  result <- ra(ctx)
  expect_equal(result$mode, "continue")
  expect_equal(result$position[["row"]], 2L)
})

test_that("resolve_action - cursor on empty first line -> continue", {
  ctx    <- make_context(c(""), 1L, 1L)
  result <- ra(ctx)
  expect_equal(result$mode, "continue")
})


# ── resolve_action: no selection — code line ──────────────────────────────────

test_that("resolve_action - cursor at end of code line (whitespace after) -> complete", {
  ctx    <- make_context(c("x <- "), 1L, 6L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
})

test_that("resolve_action - cursor on code line directly after bare %: complete with token", {
  # left_trim after whitespace strip is "x %", which ends with "%" token
  ctx    <- make_context(c("x %"), 1L, 4L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
  expect_equal(result$text, "%")
})

test_that("resolve_action - cursor on code line ending with bare %: complete with token", {
  # left_trim after whitespace strip is "x %", which ends with "%" token
  ctx    <- make_context(c("x % "), 1L, 5L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
  expect_equal(result$text, "%")
})

test_that("resolve_action - cursor on code line directly after %>: complete with token %>", {
  # left_trim after whitespace strip is "x %>", which ends with "%>" token
  ctx    <- make_context(c("x %>"), 1L, 5L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
  expect_equal(result$text, "%>")
})

test_that("resolve_action - cursor on code line ending with %>: complete with token", {
  # left_trim after whitespace strip is "x %>", which ends with "%>" token
  ctx    <- make_context(c("x %> "), 1L, 6L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
  expect_equal(result$text, "%>")
})

test_that("resolve_action - cursor on code line directly after %>%: complete with no token", {
  # left_trim after whitespace strip is "x %>%", trailing % is end of %>% not a partial token
  ctx    <- make_context(c("x %>%"), 1L, 6L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
  expect_equal(result$text, "")
})

test_that("resolve_action - cursor on code line ending with %>%: complete with no token", {
  # left_trim after whitespace strip is "x %>%", trailing % is end of %>% not a partial token
  ctx    <- make_context(c("x %>% "), 1L, 7L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
  expect_equal(result$text, "")
})

test_that("resolve_action - cursor on code line with code after -> edit", {
  ctx    <- make_context(c("x <- 1"), 1L, 3L)
  result <- ra(ctx)
  expect_equal(result$mode, "edit")
  expect_equal(result$text, "x <- 1")
})

test_that("resolve_action - cursor on inline comment -> comment", {
  ctx    <- make_context(c("x <- 1 # old comment"), 1L, 9L)
  result <- ra(ctx)
  expect_equal(result$mode, "comment")
  expect_equal(result$text[1L], "x <- 1")
  expect_equal(result$text[2L], "old comment")
})


# ── resolve_action: single-line selection — whitespace ────────────────────────

test_that("resolve_action - whitespace selection on comment line -> generate", {
  # Select only the leading spaces of an indented comment — selection is whitespace,
  # full line is a comment line
  ctx    <- make_selection_context(c("  # do something"), 1L, 1L, 1L, 3L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "do something")
})

test_that("resolve_action - whitespace selection on empty line with comment above -> generate", {
  ctx    <- make_selection_context(c("# instruction", "  "), 2L, 1L, 2L, 2L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
})

test_that("resolve_action - whitespace selection on empty line with no comment above -> continue", {
  ctx    <- make_selection_context(c("x <- 1", "  "), 2L, 1L, 2L, 2L)
  result <- ra(ctx)
  expect_equal(result$mode, "continue")
})

test_that("resolve_action - whitespace selection on code line -> complete", {
  ctx    <- make_selection_context(c("x <- "), 1L, 5L, 1L, 6L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
})


# ── resolve_action: single-line selection — non-whitespace ────────────────────

test_that("resolve_action - non-whitespace selection on comment line -> generate", {
  ctx    <- make_selection_context(c("# do something"), 1L, 3L, 1L, 15L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "do something")
})

test_that("resolve_action - non-whitespace selection on code line -> edit", {
  ctx    <- make_selection_context(c("x <- 1 + 2"), 1L, 6L, 1L, 11L)
  result <- ra(ctx)
  expect_equal(result$mode, "edit")
  expect_equal(result$text, "1 + 2")
})

test_that("resolve_action - non-whitespace selection on inline comment -> comment", {
  ctx    <- make_selection_context(c("x <- 1 # old"), 1L, 8L, 1L, 13L)
  result <- ra(ctx)
  expect_equal(result$mode, "comment")
})


# ── resolve_action: multi-line selection ──────────────────────────────────────

test_that("resolve_action - multi-line code selection -> edit", {
  ctx    <- make_selection_context(c("x <- 1", "y <- 2"), 1L, 1L, 2L, 7L)
  result <- ra(ctx)
  expect_equal(result$mode, "edit")
  expect_true(grepl("x <- 1", result$text))
  expect_true(grepl("y <- 2", result$text))
})

test_that("resolve_action - multi-line selection ending at col 1 trims trailing newline", {
  ctx    <- make_selection_context(c("x <- 1", "y <- 2", "z <- 3"), 1L, 1L, 3L, 1L)
  result <- ra(ctx)
  expect_equal(result$mode, "edit")
  expect_false(endsWith(result$text, "\n"))
})

test_that("resolve_action - multi-line code selection where anchor is start row with inline comment -> comment", {
  ctx    <- make_selection_context(c("x <- 1 # old", "  "), 1L, 8L, 2L, 2L)
  result <- ra(ctx)
  expect_equal(result$mode, "comment")
})

test_that("resolve_action - multi-line comment selection -> generate from contiguous block", {
  ctx    <- make_selection_context(c("# line one", "# line two", "x <- 1"), 1L, 1L, 2L, 11L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "line one\nline two")
})

test_that("resolve_action - multi-line comment selection with non-comment row above anchor stops block at code row", {
  # row 1 is code, rows 2-3 are comments; selection covers all three
  # anchor = row 3 (comment), walk up hits row 1 (code) -> break -> block is rows 2-3 only
  ctx    <- make_selection_context(c("x <- 1", "# line one", "# line two"), 1L, 1L, 3L, 11L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "line one\nline two")
})

test_that("resolve_action - multi-line selection with only whitespace -> continue", {
  ctx    <- make_selection_context(c("", ""), 1L, 1L, 2L, 1L)
  result <- ra(ctx)
  expect_equal(result$mode, "continue")
})

test_that("resolve_action - multi-line whitespace selection with comment start line -> generate", {
  ctx    <- make_selection_context(c("# instruction", "  ", "  "), 1L, 1L, 3L, 1L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "instruction")
})

test_that("resolve_action - multi-line whitespace selection with comment start line, anchor in whitespace -> generate", {
  ctx    <- make_selection_context(c("# instruction   ", "  "), 1L, 15L, 2L, 2L)
  result <- ra(ctx)
  expect_equal(result$mode, "generate")
  expect_equal(result$text, "instruction")
})

test_that("resolve_action - multi-line whitespace selection with code start line, anchor at comment column -> comment", {
  ctx    <- make_selection_context(c("x <- 1 # note", "  "), 1L, 8L, 2L, 2L)
  result <- ra(ctx)
  expect_equal(result$mode, "comment")
})

test_that("resolve_action - multi-line whitespace selection with code start line, anchor past inline comment -> comment", {
  ctx    <- make_selection_context(c("x <- 1 # note   ", "  "), 1L, 15L, 2L, 2L)
  result <- ra(ctx)
  expect_equal(result$mode, "comment")
})

test_that("resolve_action - multi-line whitespace selection with code start line -> complete", {
  ctx    <- make_selection_context(c("x <- 1     ", "  "), 1L, 8L, 2L, 2L)
  result <- ra(ctx)
  expect_equal(result$mode, "complete")
})


# ── resolve_action: action fields ─────────────────────────────────────────────

test_that("resolve_action - edit action has text, range, position", {
  ctx    <- make_context(c("x <- 1"), 1L, 3L)
  result <- ra(ctx)
  expect_true(all(c("text", "range", "position") %in% names(result)))
})

test_that("resolve_action - generate action has text, range, position", {
  ctx    <- make_context(c("# do something"), 1L, 1L)
  result <- ra(ctx)
  expect_true(all(c("text", "range", "position") %in% names(result)))
})

test_that("resolve_action - complete action has text, range, position", {
  ctx    <- make_context(c("x <- "), 1L, 6L)
  result <- ra(ctx)
  expect_true(all(c("text", "range", "position") %in% names(result)))
})

test_that("resolve_action - continue action has only position", {
  ctx    <- make_context(c("x <- 1", ""), 2L, 1L)
  result <- ra(ctx)
  expect_equal(names(result), c("mode", "position"))
})

test_that("resolve_action - comment action has text (length 2), range, position", {
  ctx    <- make_context(c("x <- 1 # old"), 1L, 9L)
  result <- ra(ctx)
  expect_length(result$text, 2L)
  expect_true(all(c("range", "position") %in% names(result)))
})

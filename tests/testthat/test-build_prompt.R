# ── Helpers ───────────────────────────────────────────────────────────────────

make_context <- function(contents) {
  list(id = "test", contents = contents)
}

pos <- function(row, col) rstudioapi::document_position(row, col)
rng <- function(r1, c1, r2, c2) rstudioapi::document_range(pos(r1, c1), pos(r2, c2))

make_action <- function(mode, text = NULL, position = pos(3L, 1L),
                        range = rng(3L, 1L, 3L, 6L)) {
  action <- list(mode = mode, position = position, range = range)
  if (!is.null(text)) action$text <- text
  action
}

bp <- function(context, action) build_prompt(context, action)

bp_mocks <- function(code) {
  with_mocked_bindings(
    build_prompt_workspace = function(...) "",
    code = code
  )
}


# ── Structure ─────────────────────────────────────────────────────────────────

test_that("build_prompt - returns list with system and user elements", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1")
  bp_mocks({
    result <- bp(ctx, action)
    expect_type(result, "list")
    expect_true(all(c("system", "user") %in% names(result)))
  })
})

test_that("build_prompt - system and user are character strings", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1")
  bp_mocks({
    result <- bp(ctx, action)
    expect_type(result$system, "character")
    expect_type(result$user, "character")
  })
})

test_that("build_prompt - errors on unknown mode", {
  ctx    <- make_context(c("x <- 1"))
  action <- make_action("unknown")
  bp_mocks({
    expect_error(bp(ctx, action), "Unknown mode")
  })
})


# ── Edit mode ─────────────────────────────────────────────────────────────────

test_that("build_prompt - edit: system prompt mentions 'Improve or fix'", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1")
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$system, "Improve or fix")
  })
})

test_that("build_prompt - edit: user prompt contains Target block with action text", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1")
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Target")
    expect_match(result$user, "x <- x \\+ 1")
  })
})

test_that("build_prompt - edit: user prompt contains Preamble with library call", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1")
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "library\\(ggplot2\\)")
  })
})

test_that("build_prompt - edit: user prompt contains Context before content", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1", position = pos(3L, 1L),
                        range = rng(3L, 1L, 3L, 11L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context before")
    expect_match(result$user, "x <- 1")
  })
})

test_that("build_prompt - edit: user prompt contains Context after content", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1", "x <- x * 2"))
  action <- make_action("edit", text = "x <- x + 1", position = pos(3L, 1L),
                        range = rng(3L, 1L, 3L, 11L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context after")
    expect_match(result$user, "x <- x \\* 2")
  })
})


# ── Generate mode ─────────────────────────────────────────────────────────────

test_that("build_prompt - generate: system prompt mentions 'Write R code'", {
  ctx    <- make_context(c("library(dplyr)", "# filter rows", ""))
  action <- make_action("generate", text = "filter rows", position = pos(3L, 1L),
                        range = rng(2L, 1L, 2L, 14L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$system, "Write R code")
  })
})

test_that("build_prompt - generate: user prompt contains Instruction block", {
  ctx    <- make_context(c("library(dplyr)", "# filter rows", ""))
  action <- make_action("generate", text = "filter rows", position = pos(3L, 1L),
                        range = rng(2L, 1L, 2L, 14L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Instruction")
    expect_match(result$user, "filter rows")
  })
})

test_that("build_prompt - generate: user prompt contains Context before content", {
  ctx    <- make_context(c("library(dplyr)", "x <- 1", "# filter rows", ""))
  action <- make_action("generate", text = "filter rows", position = pos(4L, 1L),
                        range = rng(3L, 1L, 3L, 14L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context before")
    expect_match(result$user, "x <- 1")
  })
})

test_that("build_prompt - generate: user prompt contains Context after content", {
  ctx    <- make_context(c("library(dplyr)", "# filter rows", "", "x <- 1"))
  action <- make_action("generate", text = "filter rows", position = pos(3L, 1L),
                        range = rng(2L, 1L, 2L, 14L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context after")
    expect_match(result$user, "x <- 1")
  })
})

test_that("build_prompt - generate: context before includes last line when position is past end of file", {
  ctx    <- make_context(c("library(dplyr)", "# filter rows"))
  action <- make_action("generate", text = "filter rows", position = pos(3L, 1L),
                        range = rng(2L, 1L, 2L, 14L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_false(grepl("NA", result$user))
    expect_match(result$user, "# filter rows")
  })
})

test_that("build_prompt - generate: no Context after block when position is past end of file", {
  ctx    <- make_context(c("library(dplyr)", "# filter rows"))
  action <- make_action("generate", text = "filter rows", position = pos(3L, 1L),
                        range = rng(2L, 1L, 2L, 14L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_false(grepl("Context after", result$user))
  })
})


# ── Complete mode ─────────────────────────────────────────────────────────────

test_that("build_prompt - complete: system prompt mentions 'Complete the last line'", {
  ctx    <- make_context(c("library(ggplot2)", "x <- ", ""))
  action <- make_action("complete", text = "", position = pos(2L, 6L),
                        range = rng(2L, 6L, 2L, 6L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$system, "Complete the last line")
  })
})

test_that("build_prompt - complete: user prompt contains Code window block", {
  ctx    <- make_context(c("library(ggplot2)", "x <- ", ""))
  action <- make_action("complete", text = "", position = pos(2L, 6L),
                        range = rng(2L, 6L, 2L, 6L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Code window")
  })
})

test_that("build_prompt - complete: RULE line present when action text is non-empty", {
  ctx    <- make_context(c("library(ggplot2)", "x %>% ", ""))
  action <- make_action("complete", text = "%>%", position = pos(2L, 7L),
                        range = rng(2L, 4L, 2L, 7L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "RULE")
    expect_match(result$user, "%>%")
  })
})

test_that("build_prompt - complete: no RULE line when action text is empty", {
  ctx    <- make_context(c("library(ggplot2)", "x <- ", ""))
  action <- make_action("complete", text = "", position = pos(2L, 6L),
                        range = rng(2L, 6L, 2L, 6L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_false(grepl("RULE", result$user))
  })
})

test_that("build_prompt - complete: user prompt contains Context after content", {
  ctx    <- make_context(c("library(ggplot2)", "x <- ", "y <- 2"))
  action <- make_action("complete", text = "", position = pos(2L, 6L),
                        range = rng(2L, 6L, 2L, 6L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context after")
    expect_match(result$user, "y <- 2")
  })
})


# ── Continue mode ─────────────────────────────────────────────────────────────

test_that("build_prompt - continue: system prompt mentions 'Write R code for a new line'", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", ""))
  action <- make_action("continue", position = pos(3L, 1L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$system, "Write R code for a new line")
  })
})

test_that("build_prompt - continue: user prompt contains Code window block", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", ""))
  action <- make_action("continue", position = pos(3L, 1L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Code window")
  })
})

test_that("build_prompt - continue: user prompt contains Context after content", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "", "y <- 2"))
  action <- make_action("continue", position = pos(3L, 1L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context after")
    expect_match(result$user, "y <- 2")
  })
})


# ── Comment mode ──────────────────────────────────────────────────────────────

test_that("build_prompt - comment: system prompt mentions 'inline comment'", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1 # old", ""))
  action <- make_action("comment", text = c("x <- 1", "old"),
                        position = pos(2L, 8L), range = rng(2L, 8L, 2L, 13L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$system, "inline comment")
  })
})

test_that("build_prompt - comment: user prompt contains Code to comment block", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1 # old", ""))
  action <- make_action("comment", text = c("x <- 1", "old"),
                        position = pos(2L, 8L), range = rng(2L, 8L, 2L, 13L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Code to comment")
    expect_match(result$user, "x <- 1")
  })
})

test_that("build_prompt - comment: Suggestion block present when existing comment non-empty", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1 # old", ""))
  action <- make_action("comment", text = c("x <- 1", "old"),
                        position = pos(2L, 8L), range = rng(2L, 8L, 2L, 13L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Suggestion")
    expect_match(result$user, "old")
  })
})

test_that("build_prompt - comment: no Suggestion block when existing comment is empty", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", ""))
  action <- make_action("comment", text = c("x <- 1", ""),
                        position = pos(2L, 7L), range = rng(2L, 7L, 2L, 7L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_false(grepl("Suggestion", result$user))
  })
})

test_that("build_prompt - comment: user prompt contains Context before content", {
  ctx    <- make_context(c("library(ggplot2)", "y <- 0", "x <- 1 # old", ""))
  action <- make_action("comment", text = c("x <- 1", "old"),
                        position = pos(3L, 8L), range = rng(3L, 8L, 3L, 13L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context before")
    expect_match(result$user, "y <- 0")
  })
})

test_that("build_prompt - comment: user prompt contains Context after content", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1 # old", "y <- 2"))
  action <- make_action("comment", text = c("x <- 1", "old"),
                        position = pos(2L, 8L), range = rng(2L, 8L, 2L, 13L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "Context after")
    expect_match(result$user, "y <- 2")
  })
})


# ── Preamble extraction ───────────────────────────────────────────────────────

test_that("build_prompt - preamble includes library() calls", {
  ctx    <- make_context(c("library(dplyr)", "library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1", position = pos(4L, 1L),
                        range = rng(4L, 1L, 4L, 11L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "library\\(dplyr\\)")
    expect_match(result$user, "library\\(ggplot2\\)")
  })
})

test_that("build_prompt - preamble reports no library calls when none present", {
  ctx    <- make_context(c("x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1", position = pos(2L, 1L),
                        range = rng(2L, 1L, 2L, 11L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "No library calls")
  })
})



# ── Line truncation ───────────────────────────────────────────────────────────

test_that("build_prompt - before: truncates to n_lines from end when context exceeds limit", {
  # 35 lines > 30-line limit; only last 30 should appear in Code window
  ctx    <- make_context(c(paste0("line", seq_len(35L)), ""))
  action <- make_action("continue", position = pos(36L, 1L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "line35")
    expect_false(grepl("line1\\b", result$user))
  })
})

test_that("build_prompt - after: truncates to n_lines from start when context exceeds limit", {
  # position at line 1; 15 lines after > 10-line limit for after()
  ctx    <- make_context(c("x <- 1", paste0("after", seq_len(15L))))
  action <- make_action("edit", text = "x <- 1", position = pos(1L, 7L),
                        range = rng(1L, 1L, 1L, 7L))
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "after1")
    expect_false(grepl("after15", result$user))
  })
})


# ── Block formatting ──────────────────────────────────────────────────────────

test_that("build_prompt - blocks use START/END delimiters", {
  ctx    <- make_context(c("library(ggplot2)", "x <- 1", "x <- x + 1"))
  action <- make_action("edit", text = "x <- x + 1")
  bp_mocks({
    result <- bp(ctx, action)
    expect_match(result$user, "\u25c0START\u25b6")
    expect_match(result$user, "\u25c0END\u25b6")
  })
})

# ── Helpers ───────────────────────────────────────────────────────────────────

pos <- function(row, col) rstudioapi::document_position(row, col)
rng <- function(r1, c1, r2, c2) rstudioapi::document_range(pos(r1, c1), pos(r2, c2))

make_context <- function(contents, id = "test") {
  list(id = id, contents = contents)
}

make_action <- function(mode, text = NULL, position = pos(1L, 1L),
                        range = rng(1L, 1L, 1L, 7L)) {
  action <- list(mode = mode, position = position, range = range)
  if (!is.null(text)) action$text <- text
  action
}

sr <- function(context, action, result,
               on_edit = function(...) invisible(NULL)) {
  with_mocked_bindings(
    .package = "rstudioapi",
    insertText             = function(...) invisible(NULL),
    setCursorPosition      = function(...) invisible(NULL),
    getSourceEditorContext = function() context,
    showEditSuggestion     = on_edit,
    code = show_result(context, action, NULL, result)
  )
}


# ── Comment mode: space prepend ───────────────────────────────────────────────

test_that("show_result - comment mode prepends space when no space before position", {
  prepended <- NULL
  ctx    <- make_context(c("x <- 1#"))
  action <- make_action("comment", text = c("x <- 1", ""),
                        position = pos(1L, 7L), range = rng(1L, 7L, 1L, 7L))
  sr(ctx, action, "assign x",
     on_edit = function(range, text, ...) { prepended <<- text; invisible(NULL) })
  expect_equal(prepended, " assign x")
})

test_that("show_result - comment mode does not prepend space when space already present", {
  prepended <- NULL
  ctx    <- make_context(c("x <- 1 #"))
  action <- make_action("comment", text = c("x <- 1", ""),
                        position = pos(1L, 8L), range = rng(1L, 8L, 1L, 8L))
  sr(ctx, action, "assign x",
     on_edit = function(range, text, ...) { prepended <<- text; invisible(NULL) })
  expect_equal(prepended, "assign x")
})


# ── Comment mode: range ───────────────────────────────────────────────────────

test_that("show_result - comment mode with empty text[2] uses showEditSuggestion with zero-width range", {
  suggestion_range <- NULL
  ctx    <- make_context(c("x <- 1 #"))
  action <- make_action("comment", text = c("x <- 1", ""),
                        position = pos(1L, 8L), range = rng(1L, 8L, 1L, 8L))
  sr(ctx, action, "assign x",
     on_edit = function(range, ...) { suggestion_range <<- range; invisible(NULL) })
  expect_equal(suggestion_range$start, suggestion_range$end)
})

test_that("show_result - comment mode with non-empty text[2] uses showEditSuggestion with non-zero range", {
  suggestion_range <- NULL
  ctx    <- make_context(c("x <- 1 # old comment"))
  action <- make_action("comment", text = c("x <- 1", "old comment"),
                        position = pos(1L, 8L), range = rng(1L, 9L, 1L, 20L))
  sr(ctx, action, "assign x",
     on_edit = function(range, ...) { suggestion_range <<- range; invisible(NULL) })
  expect_false(identical(suggestion_range$start, suggestion_range$end))
})


# ── Complete mode: space prepend ──────────────────────────────────────────────

test_that("show_result - complete mode prepends space when before ends with |>", {
  suggestion_text <- NULL
  ctx    <- make_context(c("x |>"))
  action <- make_action("complete", position = pos(1L, 5L))
  sr(ctx, action, "filter()",
     on_edit = function(range, text, ...) { suggestion_text <<- text; invisible(NULL) })
  expect_equal(suggestion_text, " filter()")
})

test_that("show_result - complete mode prepends space when before ends with %>%", {
  suggestion_text <- NULL
  ctx    <- make_context(c("x %>%"))
  action <- make_action("complete", position = pos(1L, 6L))
  sr(ctx, action, "filter()",
     on_edit = function(range, text, ...) { suggestion_text <<- text; invisible(NULL) })
  expect_equal(suggestion_text, " filter()")
})

test_that("show_result - complete mode prepends space when result starts with |>", {
  suggestion_text <- NULL
  ctx    <- make_context(c("x"))
  action <- make_action("complete", position = pos(1L, 2L))
  sr(ctx, action, "|> filter()",
     on_edit = function(range, text, ...) { suggestion_text <<- text; invisible(NULL) })
  expect_equal(suggestion_text, " |> filter()")
})

test_that("show_result - complete mode prepends space when result starts with %>%", {
  suggestion_text <- NULL
  ctx    <- make_context(c("x"))
  action <- make_action("complete", position = pos(1L, 2L))
  sr(ctx, action, "%>% filter()",
     on_edit = function(range, text, ...) { suggestion_text <<- text; invisible(NULL) })
  expect_equal(suggestion_text, " %>% filter()")
})

test_that("show_result - complete mode does not double-space when result already has leading space", {
  suggestion_text <- NULL
  ctx    <- make_context(c("x |>"))
  action <- make_action("complete", position = pos(1L, 5L))
  sr(ctx, action, " filter()",
     on_edit = function(range, text, ...) { suggestion_text <<- text; invisible(NULL) })
  expect_equal(suggestion_text, " filter()")
})

test_that("show_result - complete mode does not prepend space when before ends with space", {
  suggestion_text <- NULL
  ctx    <- make_context(c("x |> "))
  action <- make_action("complete", position = pos(1L, 6L))
  sr(ctx, action, "filter()",
     on_edit = function(range, text, ...) { suggestion_text <<- text; invisible(NULL) })
  expect_equal(suggestion_text, "filter()")
})


# ── Ghost text path (zero-width range) ───────────────────────────────────────

test_that("show_result - generate mode prepends newline when insertion point past end of file", {
  suggestion_text <- NULL
  ctx    <- make_context(c("# filter rows"))
  action <- make_action("generate", position = pos(2L, 1L),
                        range = rng(1L, 1L, 1L, 14L))
  sr(ctx, action, "df %>% filter()",
     on_edit = function(range, text, ...) { suggestion_text <<- text; invisible(NULL) })
  expect_equal(suggestion_text, "\ndf %>% filter()")
})

test_that("show_result - generate mode inserts newline when insertion line is non-empty", {
  inserted <- FALSE
  ctx    <- make_context(c("# filter rows", "existing code"))
  action <- make_action("generate", position = pos(2L, 1L),
                        range = rng(1L, 1L, 1L, 14L))
  with_mocked_bindings(
    .package = "rstudioapi",
    insertText             = function(...) { inserted <<- TRUE; invisible(NULL) },
    setCursorPosition      = function(...) invisible(NULL),
    getSourceEditorContext = function() ctx,
    showEditSuggestion     = function(...) invisible(NULL),
    code = show_result(ctx, action, NULL, "df %>% filter()")
  )
  expect_true(inserted)
})

test_that("show_result - complete mode uses showEditSuggestion with zero-width range", {
  suggestion_range <- NULL
  ctx    <- make_context(c("x <- "))
  action <- make_action("complete", position = pos(1L, 6L),
                        range = rng(1L, 6L, 1L, 6L))
  sr(ctx, action, "42",
     on_edit = function(range, ...) { suggestion_range <<- range; invisible(NULL) })
  expect_equal(suggestion_range$start, suggestion_range$end)
})

test_that("show_result - continue mode uses showEditSuggestion with zero-width range", {
  suggestion_range <- NULL
  ctx    <- make_context(c("x <- 1", ""))
  action <- make_action("continue", position = pos(2L, 1L),
                        range = rng(2L, 1L, 2L, 1L))
  sr(ctx, action, "y <- 2",
     on_edit = function(range, ...) { suggestion_range <<- range; invisible(NULL) })
  expect_equal(suggestion_range$start, suggestion_range$end)
})

test_that("show_result - generate mode uses showEditSuggestion with zero-width range", {
  suggestion_range <- NULL
  ctx    <- make_context(c("# filter rows", ""))
  action <- make_action("generate", position = pos(2L, 1L),
                        range = rng(1L, 1L, 1L, 14L))
  sr(ctx, action, "df %>% filter()",
     on_edit = function(range, ...) { suggestion_range <<- range; invisible(NULL) })
  expect_equal(suggestion_range$start, suggestion_range$end)
})


# ── Edit mode: showEditSuggestion ─────────────────────────────────────────────

test_that("show_result - edit mode uses showEditSuggestion with non-zero range", {
  suggestion_range <- NULL
  ctx    <- make_context(c("x <- 1"))
  action <- make_action("edit", text = "x <- 1")
  sr(ctx, action, "x <- 2",
     on_edit = function(range, ...) { suggestion_range <<- range; invisible(NULL) })
  expect_false(identical(suggestion_range$start, suggestion_range$end))
})


# ── Return value ──────────────────────────────────────────────────────────────

test_that("show_result - returns invisible TRUE on success", {
  ctx    <- make_context(c("x <- 1"))
  action <- make_action("edit", text = "x <- 1")
  expect_true(sr(ctx, action, "x <- 2"))
})

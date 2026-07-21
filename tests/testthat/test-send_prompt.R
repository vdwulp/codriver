# ── Helpers ───────────────────────────────────────────────────────────────────

make_chat <- function(response) {
  list(chat = function(...) response)
}

openai_cfg <- function() {
  list(name = "openai", args = list(model = "gpt-4o"))
}

make_prompt <- function(system = "sys", user = "usr") {
  list(system = system, user = user)
}

make_action <- function(mode = "edit", text = "original") {
  list(mode = mode, text = text)
}


# ── Routing ───────────────────────────────────────────────────────────────────

test_that("send_prompt - uses ellmer::chat", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("response"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(text = "other"), make_prompt())
  expect_equal(result, "response")
})


# ── Retry logic ───────────────────────────────────────────────────────────────

test_that("send_prompt - retries on HTTP 500 and succeeds", {
  attempt <- 0L
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) list(chat = function(...) {
      attempt <<- attempt + 1L
      if (attempt < 2L) stop("HTTP 500 Internal Server Error") else "recovered"
    }),
    .package = "ellmer"
  )
  local_mocked_bindings(Sys.sleep = function(...) invisible(NULL), .package = "base")
  result <- suppressMessages(send_prompt(NULL, make_action(text = "other"), make_prompt()))
  expect_equal(result, "recovered")
  expect_equal(attempt, 2L)
})

test_that("send_prompt - retries on APIConnectionError and succeeds", {
  attempt <- 0L
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) list(chat = function(...) {
      attempt <<- attempt + 1L
      if (attempt < 2L) stop("APIConnectionError: connection refused") else "recovered"
    }),
    .package = "ellmer"
  )
  local_mocked_bindings(Sys.sleep = function(...) invisible(NULL), .package = "base")
  result <- suppressMessages(send_prompt(NULL, make_action(text = "other"), make_prompt()))
  expect_equal(result, "recovered")
})

test_that("send_prompt - errors after max retries exceeded", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) list(chat = function(...) stop("HTTP 503 Service Unavailable")),
    .package = "ellmer"
  )
  local_mocked_bindings(Sys.sleep = function(...) invisible(NULL), .package = "base")
  expect_error(
    suppressMessages(send_prompt(NULL, make_action(), make_prompt())),
    "Request failed"
  )
})

test_that("send_prompt - does not retry on non-retryable error", {
  attempt <- 0L
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) list(chat = function(...) {
      attempt <<- attempt + 1L
      stop("Bad request: invalid model")
    }),
    .package = "ellmer"
  )
  expect_error(
    send_prompt(NULL, make_action(), make_prompt()),
    "Request failed"
  )
  expect_equal(attempt, 1L)
})


# ── Post-processing: general ──────────────────────────────────────────────────

test_that("send_prompt - strips fenced code block wrapper", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("```r\nx <- 1\n```"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(text = "other"), make_prompt())
  expect_equal(result, "x <- 1")
})

test_that("send_prompt - strips fenced code block without language tag", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("```\nx <- 1\n```"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(text = "other"), make_prompt())
  expect_equal(result, "x <- 1")
})

test_that("send_prompt - normalises whitespace-only response to empty string", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("   \n  "),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "edit", text = "other"), make_prompt())
  expect_equal(result, "other")
})


# ── Post-processing: complete mode ────────────────────────────────────────────

test_that("send_prompt - complete mode: empty response returns no-suggestion marker", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "complete", text = "x <-"), make_prompt())
  expect_equal(result, " # no suggestion")
})

test_that("send_prompt - complete mode: strips leading echo of existing text", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("x <- 42"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "complete", text = "x <-"), make_prompt())
  expect_equal(result, " 42")
})

test_that("send_prompt - complete mode: returns response as-is when no echo", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(" 42"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "complete", text = "x <-"), make_prompt())
  expect_equal(result, " 42")
})


# ── Post-processing: continue mode ───────────────────────────────────────────

test_that("send_prompt - continue mode: empty response returns no-suggestion marker", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "continue", text = "x <- 1"), make_prompt())
  expect_equal(result, "# no suggestion")
})


# ── Post-processing: edit mode ────────────────────────────────────────────────

test_that("send_prompt - edit mode: returns original text when response is empty", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "edit", text = "original"), make_prompt())
  expect_equal(result, "original")
})

test_that("send_prompt - edit mode: returns response when different from input", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("improved"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "edit", text = "original"), make_prompt())
  expect_equal(result, "improved")
})


# ── Post-processing: comment mode ─────────────────────────────────────────────

test_that("send_prompt - comment mode: empty response returns no-suggestion when no existing comment", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "comment", text = c("x <- 1", "")), make_prompt())
  expect_equal(result, "no suggestion")
})

test_that("send_prompt - comment mode: empty response returns existing comment when present", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "comment", text = c("x <- 1", "# existing comment")), make_prompt())
  expect_equal(result, "# existing comment")
})

test_that("send_prompt - comment mode: returns response when comment is new", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("# new comment"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL, make_action(mode = "comment", text = c("x <- 1", "# existing comment")), make_prompt())
  expect_equal(result, "# new comment")
})

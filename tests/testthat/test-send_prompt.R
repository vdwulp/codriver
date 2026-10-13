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
    chat = function(...) {
      list(chat = function(...) {
        attempt <<- attempt + 1L
        if (attempt < 2L) stop("HTTP 500 Internal Server Error")
        else "recovered"
      })
    },
    .package = "ellmer"
  )
  local_mocked_bindings(Sys.sleep = function(...) invisible(NULL),
                        .package = "base")
  result <- suppressMessages(send_prompt(NULL,
                                         make_action(text = "other"),
                                         make_prompt()))
  expect_equal(result, "recovered")
  expect_equal(attempt, 2L)
})

test_that("send_prompt - retries on APIConnectionError and succeeds", {
  attempt <- 0L
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) {
      list(chat = function(...) {
        attempt <<- attempt + 1L
        if (attempt < 2L) stop("APIConnectionError: connection refused")
        else "recovered"
      })
    },
    .package = "ellmer"
  )
  local_mocked_bindings(Sys.sleep = function(...) invisible(NULL),
                        .package = "base")
  result <- suppressMessages(send_prompt(NULL,
                                         make_action(text = "other"),
                                         make_prompt()))
  expect_equal(result, "recovered")
})

test_that("send_prompt - errors after max retries exceeded", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) {
      list(chat = function(...) stop("HTTP 503 Service Unavailable"))
    },
    .package = "ellmer"
  )
  local_mocked_bindings(Sys.sleep = function(...) invisible(NULL),
                        .package = "base")
  expect_error(
    suppressMessages(send_prompt(NULL, make_action(), make_prompt())),
    "Request failed"
  )
})

test_that("send_prompt - does not retry on non-retryable error", {
  attempt <- 0L
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) {
      list(chat = function(...) {
        attempt <<- attempt + 1L
        stop("Bad request: invalid model")
      })
    },
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
  result <- send_prompt(NULL,
                        make_action(mode = "edit", text = "other"),
                        make_prompt())
  expect_equal(result, "other")
})


# ── Post-processing: complete mode ────────────────────────────────────────────

test_that("send_prompt - complete mode: empty response returns no-suggestion marker", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "complete", text = "x <-"),
                        make_prompt())
  expect_equal(result, " # no suggestion")
})

test_that("send_prompt - complete mode: strips leading echo of existing text", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("x <- 42"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "complete", text = "x <-"),
                        make_prompt())
  expect_equal(result, " 42")
})

test_that("send_prompt - complete mode: returns response as-is when no echo", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(" 42"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "complete", text = "x <-"),
                        make_prompt())
  expect_equal(result, " 42")
})


# ── Post-processing: continue mode ───────────────────────────────────────────

test_that("send_prompt - continue mode: empty response returns no-suggestion marker", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "continue", text = "x <- 1"),
                        make_prompt())
  expect_equal(result, "# no suggestion")
})


# ── Post-processing: edit mode ────────────────────────────────────────────────

test_that("send_prompt - edit mode: returns original text when response is empty", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "edit", text = "original"),
                        make_prompt())
  expect_equal(result, "original")
})

test_that("send_prompt - edit mode: returns response when different from input", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("improved"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "edit", text = "original"),
                        make_prompt())
  expect_equal(result, "improved")
})


# ── Post-processing: comment mode ─────────────────────────────────────────────

test_that("send_prompt - comment mode: empty response returns no-suggestion when no existing comment", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "comment", text = c("x <- 1", "")),
                        make_prompt())
  expect_equal(result, "no suggestion")
})

test_that("send_prompt - comment mode: empty response returns existing comment when present", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(""),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "comment",
                                    text = c("x <- 1", "# existing comment")),
                        make_prompt())
  expect_equal(result, "# existing comment")
})

test_that("send_prompt - comment mode: returns response when comment is new", {
  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat("# new comment"),
    .package = "ellmer"
  )
  result <- send_prompt(NULL,
                        make_action(mode = "comment",
                                    text = c("x <- 1", "# existing comment")),
                        make_prompt())
  expect_equal(result, "# new comment")
})


# ── Last chat ─────────────────────────────────────────────────────────────────

test_that("set_last_chat - resets response when prompts are supplied", {
  set_last_chat()
  on.exit(set_last_chat())

  set_last_chat("old system", "old user", "old response")
  set_last_chat("new system", "new user")

  result <- codriver_last_chat()

  expect_equal(
    result,
    structure(
      list(
        system_prompt = "new system",
        user_prompt = "new user"
      ),
      class = "codriver_last_chat"
    )
  )
  expect_false("assistant_response" %in% names(codriver_last_chat()))
})

test_that("set_last_chat - preserves prompts when response is supplied", {
  set_last_chat()
  on.exit(set_last_chat())

  set_last_chat("system", "user")
  set_last_chat(assistant_response = "assistant")

  expect_equal(
    codriver_last_chat(),
    structure(
      list(
        system_prompt = "system",
        user_prompt = "user",
        assistant_response = "assistant"
      ),
      class = "codriver_last_chat"
    )
  )
})

test_that("set_last_chat - removes response when NULL is supplied", {
  set_last_chat()
  on.exit(set_last_chat())

  set_last_chat("system", "user", "assistant")
  set_last_chat(assistant_response = NULL)

  expect_false("assistant_response" %in% names(codriver_last_chat()))
})

test_that("set_last_chat - clears stored chat when called without arguments", {
  set_last_chat()
  on.exit(set_last_chat())

  set_last_chat("system", "user", "assistant")
  set_last_chat()

  expect_null(codriver_last_chat())
})

test_that("send_prompt - records the most recent chat with raw response", {
  set_last_chat()
  on.exit(set_last_chat())

  raw_response <- "```r\nresponse\n```"

  local_mocked_bindings(get_config = openai_cfg, .package = "codriver")
  local_mocked_bindings(
    chat = function(...) make_chat(raw_response),
    .package = "ellmer"
  )

  result <- send_prompt(NULL, make_action(text = "other"), make_prompt())

  expect_equal(result, "response")
  expect_equal(
    codriver_last_chat(),
    structure(
      list(
        system_prompt = "sys",
        user_prompt = "usr",
        assistant_response = raw_response
      ),
      class = "codriver_last_chat"
    )
  )
})

test_that("print.codriver_last_chat - prints stored prompts and response", {
  old_options <- options(width = 80L)
  on.exit(options(old_options))

  chat <- structure(
    list(
      system_prompt = "System prompt",
      user_prompt = "User prompt",
      assistant_response = "Assistant response"
    ),
    class = "codriver_last_chat"
  )

  output <- capture.output(print(chat))

  expect_match(output[[1]], "^── codriver last chat ")

  expect_true(any(output == "▶▶ SYSTEM PROMPT"))
  expect_true(any(output == "System prompt"))

  expect_true(any(output == "▶▶ USER PROMPT"))
  expect_true(any(output == "User prompt"))

  expect_true(any(output == "▶▶ ASSISTANT RESPONSE"))
  expect_true(any(output == "Assistant response"))

  expect_match(tail(output, 1L), "^─+$")
})


test_that("print.codriver_last_chat - prints unrecorded sections", {
  chat <- structure(
    list(
      system_prompt = NULL,
      user_prompt = NULL,
      assistant_response = NULL
    ),
    class = "codriver_last_chat"
  )

  output <- capture.output(print(chat))

  expect_true(any(output == "▶▶ SYSTEM PROMPT"))
  expect_true(any(output == "▶▶ USER PROMPT"))
  expect_true(any(output == "▶▶ ASSISTANT RESPONSE"))

  expect_equal(sum(output == "  not recorded"), 3L)
})


test_that("print.codriver_last_chat - preserves line breaks in responses", {
  chat <- structure(
    list(
      system_prompt = "System prompt",
      user_prompt = "User prompt",
      assistant_response = paste(
        "%>% mutate(x = 1)",
        "",
        "%>% filter(x > 0)",
        sep = "\n"
      )
    ),
    class = "codriver_last_chat"
  )

  output <- capture.output(print(chat))

  expect_true(any(output == "%>% mutate(x = 1)"))
  expect_true(any(output == "%>% filter(x > 0)"))

  first_line <- match("%>% mutate(x = 1)", output)
  second_line <- match("%>% filter(x > 0)", output)

  expect_equal(output[(first_line + 1L):(second_line - 1L)], "")
})

test_that("print.codriver_last_chat - wraps long lines", {
  old_options <- options(width = 80L)
  on.exit(options(old_options))

  chat <- structure(
    list(
      system_prompt = "System prompt",
      user_prompt = "User prompt",
      assistant_response = strrep("x", 80L)
    ),
    class = "codriver_last_chat"
  )

  output <- capture.output(print(chat))

  expect_true(any(output == strrep("x", 79L)))
  expect_true(any(output == "x"))
})

# ── codriver_last_request (incl. set_last_request) ────────────────────────────

test_that("codriver_last_request - resets response when prompts are supplied", {
  set_last_request()
  on.exit(set_last_request())

  set_last_request("old system", "old user", "old response")
  set_last_request("new system", "new user")

  result <- codriver_last_request()

  expect_equal(
    result,
    structure(
      list(
        system_prompt = "new system",
        user_prompt = "new user"
      ),
      class = "codriver_last_request"
    )
  )
  expect_false("assistant_response" %in% names(codriver_last_request()))
})

test_that("codriver_last_request - preserves prompts when response is added", {
  set_last_request()
  on.exit(set_last_request())

  set_last_request("system", "user")
  set_last_request(assistant_response = "assistant")

  expect_equal(
    codriver_last_request(),
    structure(
      list(
        system_prompt = "system",
        user_prompt = "user",
        assistant_response = "assistant"
      ),
      class = "codriver_last_request"
    )
  )
})

test_that("codriver_last_request - removes response when NULL is supplied", {
  set_last_request()
  on.exit(set_last_request())

  set_last_request("system", "user", "assistant")
  set_last_request(assistant_response = NULL)

  expect_false("assistant_response" %in% names(codriver_last_request()))
})

test_that("codriver_last_request - clears when called without arguments", {
  set_last_request()
  on.exit(set_last_request())

  set_last_request("system", "user", "assistant")
  set_last_request()

  expect_null(codriver_last_request())
})


# ── print.codriver_last_request ───────────────────────────────────────────────

test_that("print.codriver_last_request - prints stored prompts and response", {
  old_options <- options(width = 80L)
  on.exit(options(old_options))

  chat <- structure(
    list(
      system_prompt = "System prompt",
      user_prompt = "User prompt",
      assistant_response = "Assistant response"
    ),
    class = "codriver_last_request"
  )

  output <- capture.output(print(chat))

  expect_match(output[[1]], "^── codriver last request ")

  expect_true(any(output == "▶▶ SYSTEM PROMPT"))
  expect_true(any(output == "System prompt"))

  expect_true(any(output == "▶▶ USER PROMPT"))
  expect_true(any(output == "User prompt"))

  expect_true(any(output == "▶▶ ASSISTANT RESPONSE"))
  expect_true(any(output == "Assistant response"))

  expect_match(tail(output, 1L), "^─+$")
})


test_that("print.codriver_last_request - prints unrecorded sections", {
  chat <- structure(
    list(
      system_prompt = NULL,
      user_prompt = NULL,
      assistant_response = NULL
    ),
    class = "codriver_last_request"
  )

  output <- capture.output(print(chat))

  expect_true(any(output == "▶▶ SYSTEM PROMPT"))
  expect_true(any(output == "▶▶ USER PROMPT"))
  expect_true(any(output == "▶▶ ASSISTANT RESPONSE"))

  expect_equal(sum(output == "  not recorded"), 3L)
})


test_that("print.codriver_last_request - preserves line breaks in responses", {
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
    class = "codriver_last_request"
  )

  output <- capture.output(print(chat))

  expect_true(any(output == "%>% mutate(x = 1)"))
  expect_true(any(output == "%>% filter(x > 0)"))

  first_line <- match("%>% mutate(x = 1)", output)
  second_line <- match("%>% filter(x > 0)", output)

  expect_equal(output[(first_line + 1L):(second_line - 1L)], "")
})

test_that("print.codriver_last_request - wraps long lines", {
  old_options <- options(width = 80L)
  on.exit(options(old_options))

  chat <- structure(
    list(
      system_prompt = "System prompt",
      user_prompt = "User prompt",
      assistant_response = strrep("x", 80L)
    ),
    class = "codriver_last_request"
  )

  output <- capture.output(print(chat))

  expect_true(any(output == strrep("x", 79L)))
  expect_true(any(output == "x"))
})

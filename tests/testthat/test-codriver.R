# ── Helpers ───────────────────────────────────────────────────────────────────

run_codriver <- function(mode         = "complete",
                         action_text  = "",
                         comment_text = "",
                         check_ok     = TRUE,
                         config_ok    = TRUE) {
  messages <- character()
  calls <- list(
    context = FALSE,
    action  = FALSE,
    prompt  = FALSE,
    result  = FALSE,
    show    = FALSE
  )

  context <- list(id = "test", contents = "x <- 1")

  with_mocked_bindings(
    getSourceEditorContext = function() {
      calls$context <<- TRUE
      context
    },
    .package = "rstudioapi",
    code = with_mocked_bindings(
      check_rstudio = function() check_ok,
      get_config = function() {
        if (config_ok) list(args = list(name = "openai/gpt-4")) else NULL
      },
      resolve_action = function(ctx) {
        calls$action <<- TRUE

        pos <- list(row = 1L, column = 1L)
        range <- list(
          start = list(row = 1L, column = 1L),
          end   = list(row = 1L, column = 1L)
        )

        if (mode == "comment") {
          list(
            mode     = "comment",
            position = pos,
            text     = c(action_text, comment_text),
            range    = range
          )
        } else if (mode == "edit") {
          list(
            mode     = "edit",
            position = pos,
            text     = action_text,
            range    = range
          )
        } else {
          list(
            mode     = mode,
            position = pos,
            text     = action_text,
            range    = range
          )
        }
      },
      build_prompt = function(ctx, act) {
        calls$prompt <<- TRUE
        list(user = "USER PROMPT", system = "SYSTEM PROMPT")
      },
      send_prompt = function(ctx, act, prm) {
        calls$result <<- TRUE
        "LLM RESULT"
      },
      show_result = function(ctx, act, prm, res) {
        calls$show <<- TRUE
        invisible(NULL)
      },
      codriver_message = function(msg, ...) {
        msg <- if (length(list(...)) == 0L) msg else sprintf(msg, ...)
        messages <<- c(messages, msg)
        invisible(NULL)
      },
      .package = "codriver",
      code = {
        value <- codriver()
        list(value = value, messages = messages, calls = calls)
      }
    )
  )
}


# ── Mode messages ─────────────────────────────────────────────────────────────

test_that("codriver - emits correct message for complete mode", {
  result <- run_codriver(mode = "complete")

  expect_true(length(result$messages) >= 1L)
  expect_equal(result$messages[1], "Complete at 1:1")
  expect_true(any(grepl("Completed in ", result$messages)))
})

test_that("codriver - emits correct message for continue mode", {
  result <- run_codriver(mode = "continue")

  expect_true(length(result$messages) >= 1L)
  expect_equal(result$messages[1], "Continue at 1:1")
  expect_true(any(grepl("Completed in ", result$messages)))
})

test_that("codriver - emits correct message for generate mode", {
  result <- run_codriver(mode = "generate", action_text = "do something")

  expect_true(length(result$messages) >= 1L)
  expect_equal(
    result$messages[1],
    "Generate at 1:1 with instruction 'do something'"
  )
  expect_true(any(grepl("Completed in ", result$messages)))
})

test_that("codriver - emits correct message for edit mode without newlines", {
  result <- run_codriver(mode = "edit", action_text = "x <- 1")

  expect_true(length(result$messages) >= 1L)
  expect_equal(
    result$messages[1],
    "Edit from 1:1 to 1:1 with original code ```x <- 1```"
  )
  expect_true(any(grepl("Completed in ", result$messages)))
})

test_that("codriver - emits correct message for comment mode without original text", {
  result <- run_codriver(mode = "comment", action_text = "x <- 1", comment_text = "")

  expect_true(length(result$messages) >= 1L)
  expect_equal(
    result$messages[1],
    "Inline comment at 1:1"
  )
  expect_true(any(grepl("Completed in ", result$messages)))
})

test_that("codriver - emits correct message for comment mode with original text", {
  result <- run_codriver(
    mode         = "comment",
    action_text  = "x <- 1",
    comment_text = "original comment"
  )

  expect_true(length(result$messages) >= 1L)
  expect_equal(
    result$messages[1],
    "Inline comment at 1:1 with original text 'original comment'"
  )
  expect_true(any(grepl("Completed in ", result$messages)))
})


# ── Success and failure ───────────────────────────────────────────────────────

test_that("codriver - returns invisible TRUE on success", {
  result <- run_codriver(mode = "complete", check_ok = TRUE)

  expect_true(result$value)
})

test_that("codriver - proceeds when check_rstudio is TRUE", {
  result <- run_codriver(mode = "complete", check_ok = TRUE)

  expect_true(result$calls$context)
  expect_true(result$calls$action)
  expect_true(result$calls$prompt)
  expect_true(result$calls$result)
  expect_true(result$calls$show)
})

test_that("codriver - returns invisible FALSE when check_rstudio is FALSE", {
  result <- run_codriver(check_ok = FALSE)

  expect_false(result$value)
  expect_false(result$calls$context)
  expect_false(result$calls$action)
  expect_false(result$calls$prompt)
  expect_false(result$calls$result)
  expect_false(result$calls$show)
  expect_length(result$messages, 0L)
})

test_that("codriver - returns invisible FALSE when no configuration is found", {
  result <- run_codriver(config_ok = FALSE)

  expect_false(result$value)
  expect_false(result$calls$context)
  expect_false(result$calls$action)
  expect_false(result$calls$prompt)
  expect_false(result$calls$result)
  expect_false(result$calls$show)

  expect_length(result$messages, 1L)
  expect_match(result$messages[1], "No configuration found")
})

test_that("codriver - emits unable-to-resolve message and returns FALSE for unknown mode", {
  result <- run_codriver(mode = "unknown")

  expect_false(result$value)
  expect_true(result$calls$context)
  expect_true(result$calls$action)
  expect_false(result$calls$prompt)
  expect_false(result$calls$result)
  expect_false(result$calls$show)

  expect_length(result$messages, 1L)
  expect_equal(result$messages[1], "Unable to resolve mode")
})


# ── codriver_message ──────────────────────────────────────────────────────────

test_that("codriver_message - prefixes message without formatting arguments", {
  expect_message(
    codriver_message("Hello world"),
    "\\[codriver\\] Hello world"
  )
})

test_that("codriver_message - prefixes and formats message with arguments", {
  expect_message(
    codriver_message("Value: %s, number: %d", "x", 1L),
    "\\[codriver\\] Value: x, number: 1"
  )
})

# ── Helpers ───────────────────────────────────────────────────────────────────

configure_mocks <- function(
    rstudio_ok    = TRUE,
    shortcut      = "Ctrl+/",
    chat_ok       = TRUE,
    chat_fn       = NULL,
    shortcut_fn   = function(...) TRUE,
    set_config_fn = function(...) invisible(NULL),
    name          = "openai",
    ...) {

  env         <- new.env(parent = emptyenv())
  env$bullets <- NULL
  fake_chat   <- list(chat = function(...) "OK")

  capture_bullets <- function(...) {
    args        <- list(...)
    x           <- if (!is.null(args$x)) args$x else args[[1]]
    env$bullets <- c(env$bullets, x)
  }

  actual_chat_fn <- chat_fn %||% function(...) if (chat_ok) fake_chat else stop("chat failed")

  result <- with_mocked_bindings(
    check_rstudio          = function() rstudio_ok,
    set_config             = set_config_fn,
    is_shortcut_registered = function() shortcut,
    register_shortcut      = shortcut_fn,
    cli_progress_message   = function(...) {},
    cli_progress_done      = function(...) {},
    cli_bullets            = capture_bullets,
    code = with_mocked_bindings(
      chat     = actual_chat_fn,
      .package = "ellmer",
      code     = codriver_configure(name, ...)
    )
  )

  list(result = result, bullets = env$bullets)
}


# ── RStudio check ─────────────────────────────────────────────────────────────

test_that("codriver_configure - returns FALSE when check_rstudio fails", {
  expect_false(configure_mocks(rstudio_ok = FALSE)$result)
})

test_that("codriver_configure - returns FALSE when provider validation fails", {
  expect_false(configure_mocks(chat_ok = FALSE)$result)
})


# ── Happy path ────────────────────────────────────────────────────────────────

test_that("codriver_configure - returns TRUE invisibly on success", {
  expect_true(configure_mocks(model = "gpt-4")$result)
})

test_that("codriver_configure - openai_compatible merges model into name", {
  recorded <- NULL
  out <- configure_mocks(
    name          = "openai_compatible",
    base_url      = "https://example.com/v1",
    model         = "custom-model",
    set_config_fn = function(cfg) { recorded <<- cfg; invisible(NULL) }
  )
  expect_true(out$result)
  expect_equal(recorded$args$name, "openai_compatible/custom-model")
  expect_equal(recorded$args$base_url, "https://example.com/v1")
  expect_null(recorded$args$model)
})

test_that("codriver_configure - openai_compatible/model slash syntax keeps name", {
  recorded <- NULL
  out <- configure_mocks(
    name          = "openai_compatible/custom-model",
    base_url      = "https://example.com/v1",
    set_config_fn = function(cfg) { recorded <<- cfg; invisible(NULL) }
  )
  expect_true(out$result)
  expect_equal(recorded$args$name, "openai_compatible/custom-model")
  expect_equal(recorded$args$base_url, "https://example.com/v1")
})


# ── Shortcut logic ────────────────────────────────────────────────────────────

test_that("codriver_configure - offers and registers default shortcut when none set", {
  registered_shortcut <- NULL
  configure_mocks(
    shortcut    = NULL,
    model       = "gpt-4",
    shortcut_fn = function(s) { registered_shortcut <<- s; TRUE }
  )
  expect_equal(registered_shortcut, "Ctrl+/")
})

test_that("codriver_configure - bullets mention strongly recommended when no shortcut", {
  out <- configure_mocks(shortcut = NULL, model = "gpt-4")
  expect_true(any(grepl("strongly recommended", out$bullets)))
})

test_that("codriver_configure - bullets mention default shortcut Ctrl+/ when no shortcut", {
  out <- configure_mocks(shortcut = NULL, model = "gpt-4")
  expect_true(any(grepl("default shortcut Ctrl\\+/", out$bullets)))
})

test_that("codriver_configure - skips shortcut registration when already registered", {
  registered_called <- FALSE
  configure_mocks(
    shortcut    = "Ctrl+/",
    model       = "gpt-4",
    shortcut_fn = function(...) { registered_called <<- TRUE; TRUE }
  )
  expect_false(registered_called)
})

test_that("codriver_configure - bullets mention no shortcut registered when user declines", {
  out <- configure_mocks(
    shortcut    = NULL,
    model       = "gpt-4",
    shortcut_fn = function(...) FALSE
  )
  expect_true(any(grepl("No shortcut was registered", out$bullets)))
})

test_that("codriver_configure - bullets recommend manual assignment when shortcut declined", {
  out <- configure_mocks(
    shortcut    = NULL,
    model       = "gpt-4",
    shortcut_fn = function(...) FALSE
  )
  expect_true(any(grepl("Modify Keyboard Shortcuts", out$bullets)))
})

test_that("codriver_configure - recommends manual assignment when shortcut cleared, does not register", {
  registered_called <- FALSE
  out <- configure_mocks(
    shortcut    = "",
    model       = "gpt-4",
    shortcut_fn = function(...) { registered_called <<- TRUE; TRUE }
  )
  expect_false(registered_called)
  expect_true(any(grepl("Modify Keyboard Shortcuts", out$bullets)))
})


# ── Error message handling ────────────────────────────────────────────────────

test_that("codriver_configure - strips ANSI codes from error message", {
  out <- configure_mocks(
    model   = "gpt-4",
    chat_fn = function(...) list(chat = function(...) stop("\033[31msome error\033[0m"))
  )
  expect_true(any(grepl("Configuration failed", out$bullets)))
  expect_false(any(grepl("\033", out$bullets, fixed = TRUE)))
})

test_that("codriver_configure - strips openai_compatible hint from error message", {
  out <- configure_mocks(
    name     = "openai_compatible",
    base_url = "https://example.com/v1",
    chat_fn  = function(...) list(chat = function(...) stop(
      "bad request\n\u2139 Use `chat_openai()` if you want to use OpenAI's official API."
    ))
  )
  expect_true(any(grepl("Configuration failed", out$bullets)))
  expect_false(any(grepl("chat_openai\\(", out$bullets)))
})


# ── get_config_path ───────────────────────────────────────────────────────────

test_that("get_config_path - returns path inside R_user_dir", {
  mock_dir <- tempfile("codriver_test")
  local_mocked_bindings(
    R_user_dir = function(package, which) mock_dir,
    .package   = "codriver"
  )
  expect_equal(get_config_path(), file.path(mock_dir, "config.rds"))
})


# ── read_config ───────────────────────────────────────────────────────────────

test_that("read_config - returns NULL when file does not exist", {
  local_mocked_bindings(
    R_user_dir = function(package, which) tempfile("codriver_test"),
    .package   = "codriver"
  )
  expect_null(read_config())
})

test_that("read_config - reads back a saved config", {
  mock_dir <- tempfile("codriver_test")
  dir.create(mock_dir, recursive = TRUE)
  saveRDS(list(args = list(name = "openai/gpt-4")), file.path(mock_dir, "config.rds"))
  local_mocked_bindings(
    R_user_dir = function(package, which) mock_dir,
    .package   = "codriver"
  )
  expect_equal(read_config()$args$name, "openai/gpt-4")
})


# ── write_config ──────────────────────────────────────────────────────────────

test_that("write_config - persists config as RDS", {
  mock_dir <- tempfile("codriver_test")
  dir.create(mock_dir, recursive = TRUE)
  local_mocked_bindings(R_user_dir = function(package, which) mock_dir, .package = "codriver")
  write_config(list(args = list(name = "openai/gpt-4")))
  expect_equal(readRDS(file.path(mock_dir, "config.rds"))$args$name, "openai/gpt-4")
})

test_that("write_config - overwrites existing file", {
  mock_dir <- tempfile("codriver_test")
  dir.create(mock_dir, recursive = TRUE)
  local_mocked_bindings(R_user_dir = function(package, which) mock_dir, .package = "codriver")
  write_config(list(args = list(name = "openai")))
  write_config(list(args = list(name = "anthropic")))
  expect_equal(readRDS(file.path(mock_dir, "config.rds"))$args$name, "anthropic")
})

test_that("write_config - creates directory if missing", {
  mock_dir <- tempfile("codriver_test")
  local_mocked_bindings(R_user_dir = function(package, which) mock_dir, .package = "codriver")
  expect_false(dir.exists(mock_dir))
  write_config(list(args = list(name = "openai")))
  expect_true(dir.exists(mock_dir))
})


# ── get_config / set_config ───────────────────────────────────────────────────

reset_config <- function() {
  env <- get(".codriver_env", envir = asNamespace("codriver"))
  old <- env$config
  withr::defer(env$config <- old, envir = parent.frame())
  env$config <- NULL
}

test_that("get_config - returns NULL when config is not set", {
  reset_config()
  expect_null(get_config())
})

test_that("get_config - returns in-memory config", {
  reset_config()
  env <- get(".codriver_env", envir = asNamespace("codriver"))
  env$config <- list(args = list(name = "openai/gpt-4"))
  expect_equal(get_config()$args$name, "openai/gpt-4")
})

test_that("set_config - writes to disk and updates in-memory cache", {
  reset_config()
  mock_dir <- tempfile("codriver_test")
  dir.create(mock_dir, recursive = TRUE)
  local_mocked_bindings(R_user_dir = function(package, which) mock_dir, .package = "codriver")
  set_config(list(args = list(name = "anthropic/claude-3")))
  env <- get(".codriver_env", envir = asNamespace("codriver"))
  expect_equal(env$config$args$name, "anthropic/claude-3")
  expect_equal(readRDS(file.path(mock_dir, "config.rds"))$args$name, "anthropic/claude-3")
})

test_that("set_config - replaces entire config (no partial merge)", {
  reset_config()
  mock_dir <- tempfile("codriver_test")
  dir.create(mock_dir, recursive = TRUE)
  local_mocked_bindings(R_user_dir = function(package, which) mock_dir, .package = "codriver")
  env <- get(".codriver_env", envir = asNamespace("codriver"))
  env$config <- list(args = list(name = "openai/gpt-4"))
  set_config(list(args = list(name = "anthropic/claude-3")))
  expect_equal(env$config$args$name, "anthropic/claude-3")
})

test_that("set_config - requires a list", {
  expect_error(set_config("not_a_list"))
})


# ── is_shortcut_registered ────────────────────────────────────────────────────

test_that("is_shortcut_registered - returns NULL when addins.json missing", {
  local_mocked_bindings(rstudio_config_path = function(...) tempfile(), .package = "codriver")
  expect_null(is_shortcut_registered())
})

test_that("is_shortcut_registered - returns shortcut string when codriver::codriver is present", {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(list(`codriver::codriver` = "Ctrl+/"), tmp, auto_unbox = TRUE)
  local_mocked_bindings(rstudio_config_path = function(...) tmp, .package = "codriver")
  expect_equal(is_shortcut_registered(), "Ctrl+/")
})

test_that("is_shortcut_registered - returns NULL when codriver::codriver is absent", {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(list(`other::addin` = "Ctrl-K"), tmp, auto_unbox = TRUE)
  local_mocked_bindings(rstudio_config_path = function(...) tmp, .package = "codriver")
  expect_null(is_shortcut_registered())
})

test_that("is_shortcut_registered - returns empty string when codriver::codriver is cleared", {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(list(`codriver::codriver` = ""), tmp, auto_unbox = TRUE)
  local_mocked_bindings(rstudio_config_path = function(...) tmp, .package = "codriver")
  expect_equal(is_shortcut_registered(), "")
})

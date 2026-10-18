# ── Helpers ───────────────────────────────────────────────────────────────────

run_codriver_info <- function(config_args = NULL) {
  with_mocked_bindings(
    get_config = function() list(args = config_args),
    .package = "codriver",
    code = {
      value <- codriver_info()
      output <- capture.output(print(value))
      list(value = value, output = output)
    }
  )
}

run_codriver_info_no_packages <- function() {
  require_namespace <- base::requireNamespace

  with_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE) {
      if (package %in% c("ellmer", "rstudioapi")) return(FALSE)
      require_namespace(package, quietly = quietly)
    },
    .package = "base",
    code = run_codriver_info()
  )
}


# ── Output and return value ───────────────────────────────────────────────────

test_that("codriver_info - prints diagnostic sections when not configured", {
  result <- run_codriver_info()

  expect_true(any(grepl("^── codriver info ─", result$output)))
  expect_true(any(grepl("^version :", result$output)))
  expect_true(any(grepl("^configuration$", result$output)))
  expect_true(any(grepl("codriver not configured", result$output)))
  expect_true(any(grepl("^environment$", result$output)))
  expect_true(startsWith(tail(result$output, 1), "\u2500"))
})

test_that("codriver_info - returns diagnostic information", {
  result <- run_codriver_info()

  expect_s3_class(result$value, "codriver_info")
  expect_named(
    result$value,
    c("version", "configuration", "environment")
  )
  expect_s3_class(result$value$version, "package_version")
  expect_null(result$value$configuration)
  expect_named(
    result$value$environment,
    c("r", "rstudio", "rstudioapi", "ellmer")
  )
  expect_s3_class(result$value$environment$r, "numeric_version")
})


# ── Configuration ─────────────────────────────────────────────────────────────

test_that("codriver_info - prints provider, model, and other configuration", {
  config_args <- list(
    name        = "openai/gpt-4.1-mini",
    temperature = 0.2
  )

  result <- run_codriver_info(config_args)

  expect_identical(result$value$configuration, config_args)
  expect_true(any(grepl("provider.*: openai$", result$output)))
  expect_true(any(grepl("model.*: gpt-4.1-mini$", result$output)))
  expect_true(any(grepl("temperature.*: 0.2$", result$output)))
})

test_that("codriver_info - prints provider only when name has no slash", {
  result <- run_codriver_info(list(name = "ollama"))

  expect_true(any(grepl("provider.*: ollama$", result$output)))
  expect_false(any(grepl("^[[:space:]]+model[[:space:]]+:", result$output)))
})


# ── Optional packages ─────────────────────────────────────────────────────────

test_that("codriver_info - reports unavailable optional packages", {
  result <- run_codriver_info_no_packages()

  expect_null(result$value$environment$ellmer)
  expect_null(result$value$environment$rstudioapi)
  expect_null(result$value$environment$rstudio)
  expect_null(result$value$token_usage)
  expect_true(any(grepl("ellmer     : not installed", result$output,
                        fixed = TRUE)))
  expect_false(any(grepl("rstudioapi", result$output, fixed = TRUE)))
})

test_that("codriver_info - prints RStudio version when available", {
  skip_if_not_installed("rstudioapi")

  with_mocked_bindings(
    isAvailable = function() TRUE,
    versionInfo = function() list(long_version = "2026.01.0+100"),
    .package = "rstudioapi",
    code = {
      result <- run_codriver_info()

      expect_identical(result$value$environment$rstudio, "2026.01.0+100")
      expect_true(any(grepl(
        "RStudio    : 2026.01.0+100",
        result$output,
        fixed = TRUE
      )))
    }
  )
})

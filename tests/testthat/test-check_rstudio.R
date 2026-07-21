# ── Helpers ───────────────────────────────────────────────────────────────────

reset_check <- function() .codriver_env$check <- NULL

run_check <- function(available     = TRUE,
                      rs_version    = "2026.07.0",
                      rsapi_version = "0.19.0",
                      has_edit      = TRUE) {
  bullets <- NULL
  result <- with_mocked_bindings(
    isAvailable = function(version_needed = NULL) {
      if (is.null(version_needed)) available
      else available && package_version(rs_version) >= version_needed
    },
    getVersion  = function() rs_version,
    hasFun      = function(name) {
      if (name == "showEditSuggestion") has_edit
      else FALSE
    },
    .package = "rstudioapi",
    code = with_mocked_bindings(
      packageVersion = function(pkg, ...) {
        if (pkg == "rstudioapi") package_version(rsapi_version)
        else utils::packageVersion(pkg, ...)
      },
      cli_bullets = function(x, ...) { bullets <<- c(bullets, x) },
      .package = "codriver",
      code = check_rstudio()
    )
  )
  list(result = result, bullets = bullets)
}


# ── Fast path ─────────────────────────────────────────────────────────────────

test_that("check_rstudio - returns TRUE immediately when result already cached as TRUE", {
  reset_check()
  on.exit(reset_check())
  .codriver_env$check <- TRUE
  expect_true(check_rstudio())
})

test_that("check_rstudio - re-runs when cached result is FALSE", {
  reset_check()
  on.exit(reset_check())
  .codriver_env$check <- FALSE
  expect_true(run_check()$result)
})


# ── RStudio not available ─────────────────────────────────────────────────────

test_that("check_rstudio - returns FALSE when RStudio is not running", {
  reset_check()
  on.exit(reset_check())
  expect_false(run_check(available = FALSE)$result)
})

test_that("check_rstudio - caches FALSE when RStudio is not running", {
  reset_check()
  on.exit(reset_check())
  run_check(available = FALSE)
  expect_false(.codriver_env$check)
})

test_that("check_rstudio - bullets mention unavailable when RStudio is not running", {
  reset_check()
  on.exit(reset_check())
  out <- run_check(available = FALSE)
  expect_true(any(grepl("RStudio must be running", out$bullets)))
})


# ── Full capability ───────────────────────────────────────────────────────────

test_that("check_rstudio - returns TRUE when all conditions met", {
  reset_check()
  on.exit(reset_check())
  expect_true(run_check()$result)
})

test_that("check_rstudio - caches TRUE when all conditions met", {
  reset_check()
  on.exit(reset_check())
  run_check()
  expect_true(.codriver_env$check)
})


# ── Version checks ────────────────────────────────────────────────────────────

test_that("check_rstudio - returns FALSE when RStudio too old", {
  reset_check()
  on.exit(reset_check())
  expect_false(run_check(rs_version = "2026.03.0")$result)
})

test_that("check_rstudio - returns FALSE when rstudioapi too old", {
  reset_check()
  on.exit(reset_check())
  expect_false(run_check(rsapi_version = "0.18.0")$result)
})

test_that("check_rstudio - returns FALSE when hasFun returns FALSE", {
  reset_check()
  on.exit(reset_check())
  expect_false(run_check(has_edit = FALSE)$result)
})

test_that("check_rstudio - returns TRUE at boundary version (2026.04.0)", {
  reset_check()
  on.exit(reset_check())
  expect_true(run_check(rs_version = "2026.04.0")$result)
})


# ── Messages ──────────────────────────────────────────────────────────────────

test_that("check_rstudio - bullets contain TOO OLD for RStudio when too old", {
  reset_check()
  on.exit(reset_check())
  expect_true(any(grepl("TOO OLD", run_check(rs_version = "2026.03.0")$bullets)))
})

test_that("check_rstudio - bullets contain TOO OLD for rstudioapi when too old", {
  reset_check()
  on.exit(reset_check())
  expect_true(any(grepl("TOO OLD", run_check(rsapi_version = "0.18.0")$bullets)))
})

test_that("check_rstudio - bullets contain MISSING when versions OK but hasFun FALSE", {
  reset_check()
  on.exit(reset_check())
  expect_true(any(grepl("MISSING", run_check(has_edit = FALSE)$bullets)))
})

# Package-scoped environment: binding at build time, created on load
.codriver_env <- NULL

#' @noRd
.onLoad <- function(libname, pkgname) {
  # nocov start - no test coverage for this function

  # Create a private environment each time the package is loaded
  .codriver_env <<- new.env(parent = emptyenv())

  # Initialize config slot
  .codriver_env$config  <- read_config()

  # nocov end
}

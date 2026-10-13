# Package-scoped binding, initialized on load
.codriver_env <- NULL

#' @noRd
.onLoad <- function(libname, pkgname) {
  # nocov start - no test coverage for this function

  # Create a private environment each time the package is loaded
  assign(
    ".codriver_env",
    new.env(parent = emptyenv()),
    envir = environment(.onLoad)
  )

  # Initialize config slot
  .codriver_env$config  <- read_config()

  # nocov end
}

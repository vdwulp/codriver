#' Check minimal requirements and capabilities for RStudio integration.
#'
#' Stores a boolean in .codriver_env$check:
#'   - TRUE if showEditSuggestion is available, FALSE otherwise
#'
#' Returns the boolean value of `.codriver_env$check`.
#'
#' @importFrom cli cli_bullets
#' @importFrom rstudioapi getVersion hasFun isAvailable
#' @importFrom utils packageVersion
#'
#' @noRd
check_rstudio <- function() {
  # Fast path: success result cached
  if (!is.null(.codriver_env$check) && isTRUE(.codriver_env$check)) {
    return(invisible(TRUE))
  }

  # RStudio must be running
  if (!rstudioapi::isAvailable()) {
    cli_bullets(c("x" = "RStudio must be running, codriver unavailable."))
    .codriver_env$check <- FALSE
    return(invisible(FALSE))
  }

  rsapi_version <- packageVersion("rstudioapi")
  rs_version    <- package_version(rstudioapi::getVersion())

  # showEditSuggestion (rstudioapi >= 0.19.0, RStudio >= 2026.04.0)
  has_edit_suggestion <- rstudioapi::hasFun("showEditSuggestion")
  req_rsapi_edit      <- package_version("0.19.0")
  req_rs_min_edit     <- package_version("2026.04.0")

  ok_rsapi_edit <- rsapi_version >= req_rsapi_edit
  ok_rs_edit    <- rstudioapi::isAvailable(version_needed = req_rs_min_edit)

  result <- (
    has_edit_suggestion &&
      ok_rsapi_edit &&
      ok_rs_edit
  )

  # Show results if a problem is found
  if (!result) {
    cli_bullets(c(
      "x" = "codriver requires `showEditSuggestion` support in RStudio and rstudioapi:",
      if (ok_rs_edit) {
        c("v" = "RStudio: OK")
      } else {
        c("x" = paste0(
          "RStudio: TOO OLD - update RStudio to ",
          as.character(req_rs_min_edit),
          " or newer (you have ",
          as.character(rs_version),
          ")."
        ))
      },
      if (ok_rsapi_edit) {
        c("v" = "rstudioapi: OK")
      } else {
        c("x" = paste0(
          "rstudioapi: TOO OLD - run install.packages(\"rstudioapi\") (you have ",
          as.character(rsapi_version),
          ", need ",
          as.character(req_rsapi_edit),
          "+)."
        ))
      },
      if (ok_rs_edit && ok_rsapi_edit && !has_edit_suggestion) {
        c("x" = "hasFun(\"showEditSuggestion\"): MISSING - install the latest RStudio and run install.packages(\"rstudioapi\").")
      }
    ))
  }

  .codriver_env$check <- result
  invisible(result)
}

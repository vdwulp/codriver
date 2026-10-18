#' Collect codriver diagnostic information
#'
#' Collects the current package version, active configuration, and R/RStudio
#' environment details.
#'
#' @return A \code{codriver_info} object: a named list with elements
#'   \code{version}, \code{configuration}, and \code{environment} (a named list
#'   with \code{r}, \code{rstudio}, \code{rstudioapi}, and \code{ellmer}).
#'
#' @importFrom rlang %||%
#' @importFrom utils packageVersion capture.output
#'
#' @export
codriver_info <- function() {
  version   <- packageVersion("codriver")
  r_version <- getRversion()

  ellmer_version <-
    if (requireNamespace("ellmer", quietly = TRUE)) {
      packageVersion("ellmer")
    } else {
      NULL
    }

  rstudioapi_version <-
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      packageVersion("rstudioapi")
    } else {
      NULL
    }

  # RStudio version (only available when running inside RStudio)
  rstudio_version <-
    if (!is.null(rstudioapi_version) && rstudioapi::isAvailable()) {
      rstudioapi::versionInfo()$long_version
    } else {
      NULL
    }

  # Configuration
  config_args <- get_config()$args

  structure(
    list(
      version = version,
      configuration = config_args,
      environment = list(
        r = r_version,
        rstudio = rstudio_version,
        rstudioapi = rstudioapi_version,
        ellmer = ellmer_version
      )
    ),
    class = "codriver_info"
  )
}

#' @export
print.codriver_info <- function(x, ...) {
  rule_width <- 56L
  rule       <- strrep("\u2500", rule_width)
  header     <- strtrim(paste0("\u2500\u2500 codriver info \u2500", rule),
                        rule_width)

  cat(header, "\n")
  cat("version :", format(x$version), "\n\n")

  cat("configuration\n")
  if (is.null(x$configuration)) {
    cat("  codriver not configured, run codriver_configure() first\n")
  } else {
    config_args <- x$configuration
    other_args <- config_args[names(config_args) != "name"]
    key_width  <- max(nchar(c("provider", "model", names(other_args))))
    name_parts <- strsplit(config_args$name, "/", fixed = TRUE)[[1]]
    cat(paste0("  ", formatC("provider", width = key_width, flag = "-"), " : ",
               name_parts[[1]], "\n"))
    if (length(name_parts) >= 2L) {
      cat(paste0("  ", formatC("model", width = key_width, flag = "-"), " : ",
                 paste(name_parts[-1], collapse = "/"), "\n"))
    }
    for (nm in names(other_args)) {
      cat(paste0("  ", formatC(nm, width = key_width, flag = "-"), " : ",
                 other_args[[nm]] %||% "<NULL>", "\n"))
    }
  }
  cat("\n")

  cat("environment\n")
  cat("  R          :", format(x$environment$r), "\n")
  if (!is.null(x$environment$rstudio)) {
    cat("  RStudio    :", x$environment$rstudio, "\n")
  }
  if (!is.null(x$environment$rstudioapi)) {
    cat("  rstudioapi :", format(x$environment$rstudioapi), "\n")
  }
  if (!is.null(x$environment$ellmer)) {
    cat("  ellmer     :", format(x$environment$ellmer), "\n")
  } else {
    cat("  ellmer     : not installed\n")
  }

  cat(rule, "\n")

  invisible(x)
}

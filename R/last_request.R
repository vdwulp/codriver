#' Inspect the most recent codriver request
#'
#' Returns the prompts and raw assistant response recorded for the most recent
#' codriver request in the current R session.
#'
#' @return A \code{codriver_last_request} object or \code{NULL} if no request
#'   has been recorded. The object is a named list that may contain:
#'   \describe{
#'     \item{system_prompt}{The system prompt used for the request.}
#'     \item{user_prompt}{The user prompt sent to the language model.}
#'     \item{assistant_response}{The raw response returned by the language
#'       model.}
#'   }
#'
#' @export
codriver_last_request <- function() {
  last_request <- .codriver_env$last_request

  if (!is.null(last_request)) {
    class(last_request) <- "codriver_last_request"
  }

  last_request
}

#' @export
print.codriver_last_request <- function(x, ...) {
  rule_width <- max(56L, getOption("width") - 1L)
  rule <- strrep("\u2500", rule_width)
  header <- strtrim(
    paste0("\u2500\u2500 codriver last request \u2500", rule),
    rule_width
  )

  write_wrapped <- function(value, width) {
    lines <- strsplit(value, "\n", fixed = TRUE)[[1]]

    for (line in lines) {
      starts <- seq.int(1L, max(1L, nchar(line)), by = width)
      writeLines(substring(line, starts, starts + width - 1L))
    }
  }

  print_section <- function(title, value) {
    cat("\n\u25b6\u25b6 ", toupper(title), "\n", sep = "")

    if (is.null(value)) {
      cat("  not recorded\n")
    } else {
      write_wrapped(value, rule_width)
    }
  }

  cat(header, "\n")

  print_section("system prompt", x$system_prompt)
  print_section("user prompt", x$user_prompt)
  print_section("assistant response", x$assistant_response)

  cat("\n", rule, "\n", sep = "")

  invisible(x)
}

#' Set the last request details
#'
#' Stores prompts and responses in the package environment. Calling the function
#' without arguments clears the stored request. Supplying a system or user
#' prompt starts a 'new request' and removes any previous assistant response.
#' Supplying only an assistant response preserves the existing prompts.
#'
#' @param system_prompt Optional system prompt.
#' @param user_prompt Optional user prompt.
#' @param assistant_response Optional assistant response.
#'
#' @noRd
set_last_request <- function(system_prompt = NULL,
                             user_prompt = NULL,
                             assistant_response = NULL) {
  if (missing(system_prompt) &&
        missing(user_prompt) &&
        missing(assistant_response)) {
    .codriver_env$last_request <- NULL
    return(invisible())
  }

  if (!missing(system_prompt) || !missing(user_prompt)) {
    .codriver_env$last_request <- list(
      system_prompt      = system_prompt,
      user_prompt        = user_prompt
    )
  }

  if (!missing(assistant_response)) {
    .codriver_env$last_request$assistant_response <- assistant_response
  }

  invisible()
}

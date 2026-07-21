#' Build workspace block for user prompt
#'
#' @param text The text in the user prompt code window.
#'
#' Describes global environment objects as a metadata string. Referenced objects
#' are always included with full descriptions. Unreferenced objects fill
#' remaining budget (up to 10 total), prioritised by type (data.frame > named >
#' bare vector) then size. Unreferenced objects get full descriptions only when
#' no objects are referenced, simple descriptions otherwise.
#'
#' @importFrom stats setNames
#' @importFrom utils head
#'
#' @noRd
build_prompt_workspace <- function(text, env = globalenv()) {
  all_names <- ls(env)

  if (length(all_names) == 0L) return("")

  # Extract tokens that match an object name in the global environment
  tokens    <- strsplit(paste(text, collapse = "\n"), "[^A-Za-z0-9._]+", perl = TRUE)[[1L]]
  mentioned <- unique(tokens[tokens %in% all_names])

  # Determine how many unreferenced objects to include
  n_extra      <- max(0L, 10L - length(mentioned))
  unreferenced <- setdiff(all_names, mentioned)

  # Select unreferenced objects by type priority then size descending.
  # Priority: data.frame/tibble = 1, named objects = 2, bare vectors = 3.
  # Size: nrow*ncol for data.frames, length() for everything else.
  if (n_extra > 0L && length(unreferenced) > 0L) {
    priority <- vapply(unreferenced, function(n) {
      x <- get(n, envir = env)
      if (inherits(x, "data.frame")) 1L
      else if (length(names(x)) > 0L) 2L
      else 3L
    }, integer(1L))

    size <- vapply(unreferenced, function(n) {
      x <- get(n, envir = env)
      if (inherits(x, "data.frame")) nrow(x) * ncol(x)
      else length(x)
    }, numeric(1L))

    unreferenced <- unreferenced[order(priority, -size)]
    unreferenced <- head(unreferenced, n_extra)
  } else {
    unreferenced <- character(0L)
  }

  # Describe all selected objects. Referenced objects get full descriptions;
  # unreferenced objects get simple descriptions when at least one referenced
  # objects exists, full descriptions otherwise.
  targets <- c(
    if (length(mentioned)     > 0L) setNames(rep(FALSE,                  length(mentioned)),     mentioned),
    if (length(unreferenced)  > 0L) setNames(rep(length(mentioned) > 0L, length(unreferenced)),  unreferenced)
  )

  # Get primary class and description of targets
  classes  <- vapply(names(targets), function(n) class(get(n, envir = env))[1L], character(1L))
  descs    <- vapply(names(targets), function(n) describe_variable(get(n, envir = env), n, simple = targets[[n]]), character(1L))

  # Build one line per class: "class: desc | desc | ..."
  by_class <- split(descs, classes)
  lines    <- mapply(function(cls, ds) paste0(cls, ": ", paste(ds, collapse = " | ")), names(by_class), by_class)
  paste(lines, collapse = "\n")
}


#' Describe a single variable as a metadata string
#'
#' @param x     The object to describe.
#' @param name  The object name as it appears in the environment.
#' @param simple If TRUE, emit minimal description (dims/length only).
#'
#' Full descriptions (simple = FALSE) include column or element names grouped
#' by type. Wide data frames (> 10 columns) use describe_columns() for compact
#' pattern strings. Named non-data-frame objects list elements by type.
#' Simple descriptions (simple = TRUE) include dimensions for data frames and
#' length for everything else. No data values are ever included — metadata only.
#'
#' @noRd
describe_variable <- function(x, name, simple = FALSE) {
  widedf_threshold <- 10L

  if (inherits(x, "data.frame")) {

    if (simple) {

      # Simple dataframe -> dimensions only
      paste0(name, " [", nrow(x), "x", ncol(x), "]")

    } else {

      # Full dataframe -> describe columns by primary class
      col_types <- vapply(x, function(col) class(col)[1L], character(1L))
      by_type   <- split(names(x), col_types)

      type_strs <- vapply(names(by_type), function(type) {
        col_str <- if (ncol(x) > widedf_threshold) {
          describe_columns(by_type[[type]])       # wide   -> discover patterns
        } else {
          paste(by_type[[type]], collapse = ", ") # narrow -> flat column list
        }
        paste0(type, ": ", col_str)
      }, character(1L))

      paste0(name, " [", nrow(x), "x", ncol(x), " ", paste(type_strs, collapse = " | "), "]")
    }

  } else {

    nms     <- names(x)
    if (!simple && length(nms) > 0L) {

      # Named object -> group elements by type
      types   <- vapply(nms, function(n) class(x[[n]])[1L], character(1L))
      by_type <- split(nms, types)
      items   <- paste(names(by_type), vapply(by_type, paste, character(1L), collapse = ", "), sep = ": ", collapse = " | ")
      paste0(name, " [", length(x), " ", items, "]")

    } else {

      # Simple or unnamed —> length only
      paste0(name, " [", length(x), "]")

    }

  }
}


#' Describe column naming patterns as a compact inline string
#'
#' @param cols Character vector of column names, typically all columns of one
#'   type within a data frame.
#'
#' Discovers prefix and suffix patterns within \code{cols} and renders them
#' into a single compact string. Patterns are identified by grouping on the
#' first or last token, then extending the shared label greedily at character
#' level. Variations (the remainder after stripping the shared stem) are listed
#' in braces: \code{amt_* {-> paid, due}}. A stem that is itself a column name
#' produces an empty-string variation rendered as \code{""}.
#' Columns not captured by any pattern are listed flat as residuals.
#'
#' Suffix patterns are discarded when exclusively owned by a single prefix
#' group, or when they span all columns in the type (universal suffix, no
#' discriminating value). Long variation lists are truncated with
#' \code{+n more}.
#'
#' @importFrom utils tail
#'
#' @noRd
describe_columns <- function(cols) {

  # ── Helpers ─────────────────────────────────────────────────────────────────

  # Splits a single column name into structural tokens, case preserved
  tokenize_colname <- function(x) {
    x <- gsub("([a-z])([A-Z])", "\\1 \\2", x)        # camelCase or PascalCase
    x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", x)  # CAPITALCase
    x <- gsub("[._:-]+", " ", x)                     # separators (- last to avoid misinterpretation)
    x <- gsub("([A-Za-z])([0-9])", "\\1 \\2", x)     # letter-digit boundary
    x <- gsub("([0-9])([A-Za-z])", "\\1 \\2", x)     # digit-letter boundary
    toks <- strsplit(x, "\\s+")[[1]]
    toks[nzchar(toks)]
  }

  # Check for pure numeric tokens
  is_numeric_token <- function(x) grepl("^[0-9]+$", x)

  # Gets the prefix (first) token from tokenized names, NA if pure numeric
  prefix_token <- function(x) {
    t <- tokens[[x]][1L]
    if (is_numeric_token(t)) NA_character_ else t
  }

  # Gets the suffix (last) token from tokenized names, NA if pure numeric
  suffix_token <- function(x) {
    t <- tail(tokens[[x]], 1L)
    if (is_numeric_token(t)) NA_character_ else t
  }

  # Finds longest common prefix across all strings in x, independent of tokens
  shared_prefix_chars <- function(x) {
    chars   <- strsplit(x, "")
    min_len <- min(lengths(chars))

    i <- 0L
    while (i < min_len && length(unique(vapply(chars, `[`, character(1L), i + 1L))) == 1L) {
      i <- i + 1L
    }

    substr(x[1L], 1L, i)
  }

  # Finds longest common suffix across all strings in x, independent of tokens
  shared_suffix_chars <- function(x) {
    # Reverse each string, find shared _prefix_, reverse back
    rev_x <- vapply(x, function(s) paste(rev(strsplit(s, "")[[1L]]), collapse = ""), character(1L))
    rev_shared <- shared_prefix_chars(rev_x)
    paste(rev(strsplit(rev_shared, "")[[1L]]), collapse = "")
  }


  # Renders a pattern into a token + variations string e.g.
  # "amt_* {-> paid, due, total}", empty variation where variable name equals
  # prefix/suffix are rendered as '""' to signal explicit.
  format_pattern <- function(p) {
    var_cap      <- 4L
    vars         <- p$variations
    vars_display <- ifelse(vars == "", '""', vars)

    if (length(vars_display) > var_cap) {
      shown <- vars_display[seq_len(var_cap)]
      extra <- length(vars_display) - var_cap
      var_str <- paste0(paste(shown, collapse = ", "), " +", extra, " more")
    } else {
      var_str <- paste(vars_display, collapse = ", ")
    }

    paste0(p$token, " {-> ", var_str, "}")
  }

  # ── General ─────────────────────────────────────────────────────────────────

  # No pattern recognition when group is small
  min_group_size <- 2L
  if (length(cols) < min_group_size) return(paste(cols, collapse = ", "))

  # Tokenize column names and store by name, avoiding repeated calls
  tokens <- lapply(cols, tokenize_colname)
  names(tokens) <- cols

  # ── Prefix patterns ─────────────────────────────────────────────────────────
  prefix_map <- split(cols, sapply(cols, prefix_token)) # group columns by first token
  prefix_map <- prefix_map[!is.na(names(prefix_map))]   # remove groups with NA prefix

  prefix_patterns <- list()
  for (p in names(prefix_map)) {
    group <- prefix_map[[p]]
    if (length(group) < min_group_size) next    # skip small group
    stem <- shared_prefix_chars(group)          # stem is extended shared prefix

    # Variations are everything after stem
    variations <- vapply(group, function(col) {
      substr(col, nchar(stem) + 1L, nchar(col))
    }, character(1L))
    variations <- unique(unname(variations))

    # Add pattern to list
    prefix_patterns[[length(prefix_patterns) + 1L]] <- list(
      token = paste0(stem, "*"),
      type  = "prefix",
      cols  = group,
      variations = variations
    )
  }

  # ── Suffix patterns ─────────────────────────────────────────────────────────
  suffix_map <- split(cols, sapply(cols, suffix_token)) # group columns by last token
  suffix_map <- suffix_map[!is.na(names(suffix_map))]   # remove groups with NA suffix

  suffix_patterns <- list()
  for (s in names(suffix_map)) {
    group <- suffix_map[[s]]
    if (length(group) < min_group_size) next    # skip small group

    # Suffix exclusively owned by one prefix —> discard
    first_tokens <- unique(vapply(group, function(col) tokens[[col]][1L], character(1L)))
    if (length(first_tokens) == 1L) next

    # Suffix cover all columns in type group —> discard
    if (length(group) == length(cols)) next

    stem <- shared_suffix_chars(group)          # stem is extended shared suffix

    # Variations are everything before stem
    variations <- vapply(group, function(col) {
      substr(col, 1L, nchar(col) - nchar(stem))
    }, character(1L))
    variations <- unique(unname(variations))

    # Add pattern to list
    suffix_patterns[[length(suffix_patterns) + 1L]] <- list(
      token = paste0("*", stem),
      type  = "suffix",
      cols  = group,
      variations = variations
    )
  }

  # ── Residuals and formatting ────────────────────────────────────────────────

  # Residuals are columns not captured by any pattern
  pattern_cols <- unique(unlist(lapply(c(prefix_patterns, suffix_patterns), `[[`, "cols")))
  residuals    <- setdiff(cols, pattern_cols)

  # Render all patterns and residuals into a single comma-separated string
  pattern_strs <- Filter(Negate(is.null), lapply(c(prefix_patterns, suffix_patterns), format_pattern))
  paste(c(pattern_strs, residuals), collapse = ", ")
}

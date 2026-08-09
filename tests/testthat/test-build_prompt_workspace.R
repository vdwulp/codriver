# ── Helpers ───────────────────────────────────────────────────────────────────

# build_prompt_workspace accepts an env argument (default globalenv()). Tests
# always pass a controlled environment so they are fully hermetic.

make_env <- function(...) {
  objs <- list(...)
  e    <- new.env(parent = emptyenv())
  for (nm in names(objs)) assign(nm, objs[[nm]], envir = e)
  e
}

bpw <- function(text, ...) build_prompt_workspace(text, make_env(...))
dv  <- function(x, name, simple = FALSE) describe_variable(x, name, simple)
dc  <- function(cols) describe_columns(cols)


# ── build_prompt_workspace - empty environment ────────────────────────────────

test_that("build_prompt_workspace - returns empty string for empty environment", {
  expect_equal(bpw("x <- 1"), "")
})


# ── build_prompt_workspace - referenced objects ───────────────────────────────

test_that("build_prompt_workspace - referenced object gets full description", {
  result <- bpw("df", df = data.frame(a = 1:3, b = letters[1:3]))
  expect_match(result, "data.frame")
  expect_match(result, "df \\[3x2")
  expect_match(result, "integer: a")
  expect_match(result, "character: b")
})

test_that("build_prompt_workspace - referenced object: all column names appear", {
  result <- bpw("df$price", df = data.frame(price = 1:5, label = letters[1:5]))
  expect_match(result, "price")
  expect_match(result, "label")
})

test_that("build_prompt_workspace - multiple referenced objects all appear", {
  result <- bpw("df1; df2",
                df1 = data.frame(x = 1:3),
                df2 = data.frame(y = 4:6))
  expect_match(result, "df1")
  expect_match(result, "df2")
})

test_that("build_prompt_workspace - token split on non-alphanumeric characters", {
  result <- bpw("result <- my_vec + 1", my_vec = 1:5)
  expect_match(result, "my_vec")
})

test_that("build_prompt_workspace - token match is exact: prefix of a name is not a match", {
  # "vec" in text must not cause "vec_long" to be treated as referenced
  result <- bpw("vec + 1", vec = 1:3, vec_long = 1:10)
  # vec_long is unreferenced -> simple description (length only)
  expect_match(result, "vec_long \\[10\\]")
  expect_false(grepl("integer: vec_long", result))
})


# ── build_prompt_workspace - unreferenced objects ─────────────────────────────

test_that("build_prompt_workspace - unreferenced object appears when budget allows", {
  result <- bpw("df", df = data.frame(a = 1:3), vec = 1:5)
  expect_match(result, "vec")
})

test_that("build_prompt_workspace - unreferenced object gets simple description when referenced object exists", {
  result <- bpw("df",
                df  = data.frame(a = 1:3, b = 4:6),
                vec = c(x = 1, y = 2, z = 3))
  # simple = TRUE for unreferenced -> length only, no element names
  expect_match(result, "vec \\[3\\]")
  expect_false(grepl("numeric: x", result))
})

test_that("build_prompt_workspace - unreferenced object gets full description when nothing is referenced", {
  result <- bpw("1 + 1", vec = c(x = 1, y = 2, z = 3))
  # simple = FALSE when no referenced objects exist
  expect_match(result, "numeric: x, y, z")
})

test_that("build_prompt_workspace - budget: at most 10 unreferenced objects shown", {
  # 15 objects, none referenced; only 10 should appear
  result <- do.call(bpw, c(list(text = "1 + 1"),
                           setNames(as.list(seq_len(15L)), paste0("v", seq_len(15L)))))
  matches <- regmatches(result, gregexpr("\\bv[0-9]+\\b", result))[[1L]]
  expect_lte(length(unique(matches)), 10L)
})

test_that("build_prompt_workspace - budget: referenced objects do not consume unreferenced slots", {
  # 9 referenced scalars leave a budget of 1; the single unreferenced object must appear
  ref   <- setNames(as.list(seq_len(9L)), paste0("r", seq_len(9L)))
  extra <- list(extra_obj = 42L)
  text  <- paste(names(ref), collapse = " ")
  result <- do.call(bpw, c(list(text = text), ref, extra))
  expect_match(result, "extra_obj")
})

test_that("build_prompt_workspace - budget: unreferenced objects beyond 10 are dropped", {
  # 10 referenced + 5 unreferenced; unreferenced budget = max(0, 10-10) = 0
  ref   <- setNames(as.list(seq_len(10L)), paste0("r", seq_len(10L)))
  unref <- setNames(as.list(seq_len(5L)),  paste0("u", seq_len(5L)))
  text  <- paste(names(ref), collapse = " ")
  result <- do.call(bpw, c(list(text = text), ref, unref))
  for (nm in names(unref)) expect_false(grepl(nm, result))
})


# ── build_prompt_workspace - priority ordering ────────────────────────────────

test_that("build_prompt_workspace - priority: data.frame before named vector before bare vector", {
  result <- bpw("1 + 1",
                bare  = 1:20,
                named = setNames(1:5, letters[1:5]),
                df    = data.frame(a = 1:3))
  df_pos    <- regexpr("\\bdf\\b",    result)
  named_pos <- regexpr("\\bnamed\\b", result)
  bare_pos  <- regexpr("\\bbare\\b",  result)
  expect_gt(df_pos,    0L)
  expect_gt(named_pos, 0L)
  expect_gt(bare_pos,  0L)
  expect_lt(df_pos,    named_pos)
  expect_lt(named_pos, bare_pos)
})

test_that("build_prompt_workspace - priority: larger data.frame before smaller", {
  result    <- bpw("1 + 1",
                   small_df = data.frame(a = 1:2),
                   large_df = data.frame(a = 1:100, b = 1:100, c = 1:100))
  large_pos <- regexpr("large_df", result)
  small_pos <- regexpr("small_df", result)
  expect_gt(large_pos, 0L)
  expect_gt(small_pos, 0L)
  expect_lt(large_pos, small_pos)
})

test_that("build_prompt_workspace - priority: larger bare vector before smaller", {
  result    <- bpw("1 + 1", short_vec = 1:3, long_vec = 1:50)
  long_pos  <- regexpr("long_vec",  result)
  short_pos <- regexpr("short_vec", result)
  expect_gt(long_pos,  0L)
  expect_gt(short_pos, 0L)
  expect_lt(long_pos, short_pos)
})


# ── build_prompt_workspace - output format ────────────────────────────────────

test_that("build_prompt_workspace - one line per primary class", {
  result <- bpw("1 + 1", df = data.frame(a = 1:3), vec = 1:5)
  lines  <- strsplit(result, "\n")[[1L]]
  expect_length(lines, 2L)
  expect_true(all(grepl("^[A-Za-z._]+:", lines)))
})

test_that("build_prompt_workspace - multiple objects of same class joined by ' | '", {
  result <- bpw("1 + 1", df1 = data.frame(a = 1:3), df2 = data.frame(b = 4:6))
  lines  <- strsplit(result, "\n")[[1L]]
  expect_length(lines, 1L)
  expect_match(lines[1L], "\\|")
})

test_that("build_prompt_workspace - returns single character scalar", {
  result <- bpw("df", df = data.frame(a = 1:3))
  expect_type(result, "character")
  expect_length(result, 1L)
})


# ── describe_variable - data.frame, simple = TRUE ────────────────────────────

test_that("describe_variable - df simple: format is 'name [NxM]'", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  expect_equal(dv(df, "mydf", simple = TRUE), "mydf [5x2]")
})

test_that("describe_variable - df simple: no column names in output", {
  result <- dv(data.frame(price = 1:5, label = letters[1:5]), "df", simple = TRUE)
  expect_false(grepl("price", result))
  expect_false(grepl("label", result))
})


# ── describe_variable - data.frame, simple = FALSE, narrow (<= 10 cols) ───────

test_that("describe_variable - df full narrow: format is 'name [NxM type: cols ...]'", {
  result <- dv(data.frame(a = 1:3, b = letters[1:3]), "df")
  expect_match(result, "^df \\[3x2 ")
})

test_that("describe_variable - df full narrow: column names grouped by type", {
  result <- dv(data.frame(x = 1:3, y = 4:6, label = letters[1:3]), "df")
  expect_match(result, "integer: x, y")
  expect_match(result, "character: label")
})

test_that("describe_variable - df full narrow: multiple types separated by ' | '", {
  result <- dv(data.frame(n = 1:3, s = letters[1:3]), "df")
  expect_match(result, "\\|")
})

test_that("describe_variable - df full narrow: single-type df has no ' | '", {
  result <- dv(data.frame(a = 1:3, b = 4:6, c = 7:9), "df")
  expect_false(grepl("\\|", result))
})

test_that("describe_variable - df full narrow: exactly 10 columns uses flat list, no pattern notation", {
  cols   <- paste0("col", seq_len(10L))
  df     <- setNames(as.data.frame(matrix(1L, nrow = 2L, ncol = 10L)), cols)
  result <- dv(df, "df")
  expect_false(grepl("\\{->", result))
  expect_match(result, "col1")
  expect_match(result, "col10")
})


# ── describe_variable - data.frame, simple = FALSE, wide (> 10 cols) ──────────

test_that("describe_variable - df full wide: uses pattern notation", {
  cols   <- paste0("amt_", c("paid", "due", "refund", "tax", "fee",
                             "gross", "net", "total", "base", "extra", "misc"))
  df     <- setNames(as.data.frame(matrix(1L, nrow = 2L, ncol = length(cols))), cols)
  result <- dv(df, "df")
  expect_match(result, "\\{->")
})

test_that("describe_variable - df full wide: still includes name and dimensions", {
  cols   <- paste0("x_", seq_len(11L))
  df     <- setNames(as.data.frame(matrix(1L, nrow = 3L, ncol = 11L)), cols)
  result <- dv(df, "df")
  expect_match(result, "df \\[3x11")
})


# ── describe_variable - named non-data.frame ──────────────────────────────────

test_that("describe_variable - named list full: format is 'name [len type: els | ...]'", {
  result <- dv(list(a = 1L, b = 2L, x = "hello"), "lst")
  expect_match(result, "lst \\[3 ")
  expect_match(result, "integer: a, b")
  expect_match(result, "character: x")
  expect_match(result, "\\|")
})

test_that("describe_variable - named vector full: groups elements by type", {
  result <- dv(c(a = 1, b = 2, c = 3), "vec")
  expect_match(result, "numeric: a, b, c")
})

test_that("describe_variable - named object simple: format is 'name [len]' only", {
  result <- dv(c(a = 1, b = 2, c = 3), "vec", simple = TRUE)
  expect_equal(result, "vec [3]")
})


# ── describe_variable - unnamed / bare object ─────────────────────────────────

test_that("describe_variable - unnamed vector: format is 'name [len]'", {
  expect_equal(dv(1:7, "vec"), "vec [7]")
})

test_that("describe_variable - unnamed vector: simple and full are identical", {
  vec <- 1:4
  expect_equal(dv(vec, "vec", simple = TRUE), dv(vec, "vec", simple = FALSE))
})

test_that("describe_variable - length-1 scalar: format is 'name [1]'", {
  expect_equal(dv(42, "x"), "x [1]")
})


# ── describe_columns - fewer than 2 columns ───────────────────────────────────

test_that("describe_columns - single column: returned as-is", {
  expect_equal(dc("price"), "price")
})

test_that("describe_columns - two unrelated columns: flat, no pattern notation", {
  result <- dc(c("price", "label"))
  expect_match(result, "price")
  expect_match(result, "label")
  expect_false(grepl("\\{->", result))
})


# ── describe_columns - prefix patterns ────────────────────────────────────────

test_that("describe_columns - detects prefix pattern and uses 'stem* {-> ...}' notation", {
  result <- dc(c("amt_paid", "amt_due", "amt_tax"))
  expect_match(result, "amt_\\*")
  expect_match(result, "\\{->")
  expect_match(result, "paid")
  expect_match(result, "due")
  expect_match(result, "tax")
})

test_that("describe_columns - stem is the longest shared character prefix of the group", {
  # "sales_q1" and "sales_q2" share "sales_q", not just "sales_"
  result <- dc(c("sales_q1", "sales_q2"))
  expect_match(result, "sales_q\\*")
})

test_that("describe_columns - stem-equal column name produces empty-string variation '\"\"'", {
  result <- dc(c("amt", "amt_paid", "amt_due"))
  expect_match(result, '""')
})

test_that("describe_columns - variations beyond 4 are truncated with '+n more'", {
  result <- dc(paste0("x_", letters[1:6]))
  expect_match(result, "\\+2 more")
})

test_that("describe_columns - exactly 4 variations are not truncated", {
  result <- dc(paste0("x_", letters[1:4]))
  expect_false(grepl("more", result))
})


# ── describe_columns - suffix patterns ────────────────────────────────────────

test_that("describe_columns - detects suffix pattern when columns share a last token across prefixes", {
  result <- dc(c("sales_usd", "cost_usd", "revenue_usd", "tax_eur"))
  expect_match(result, "\\*_usd")
  expect_match(result, "\\{->")
})

test_that("describe_columns - suffix owned by a single prefix group is discarded", {
  # All _usd columns start with "amt" -> single prefix owner -> discard
  result <- dc(c("amt_a_usd", "amt_b_usd", "other_eur"))
  expect_false(grepl("\\*_usd", result))
})

test_that("describe_columns - suffix spanning all columns is discarded", {
  result <- dc(c("a_val", "b_val", "c_val"))
  expect_false(grepl("\\*_val", result))
})


# ── describe_columns - residuals ──────────────────────────────────────────────

test_that("describe_columns - columns not captured by any pattern are listed as residuals", {
  result <- dc(c("amt_paid", "amt_due", "standalone"))
  expect_match(result, "standalone")
})

test_that("describe_columns - all columns become residuals when no pattern qualifies", {
  result <- dc(c("alpha", "beta", "gamma"))
  expect_false(grepl("\\{->", result))
  expect_match(result, "alpha")
  expect_match(result, "beta")
  expect_match(result, "gamma")
})


# ── describe_columns - tokenization ───────────────────────────────────────────

test_that("describe_columns - camelCase split at case boundary, prefix pattern detected", {
  result <- dc(c("amtPaid", "amtDue", "amtTax"))
  expect_match(result, "\\{->")
  expect_match(result, "Paid")
  expect_match(result, "Due")
  expect_match(result, "Tax")
})

test_that("describe_columns - dot separator treated as token boundary", {
  result <- dc(c("sales.q1", "sales.q2", "sales.q3"))
  expect_match(result, "\\{->")
})

test_that("describe_columns - underscore separator treated as token boundary", {
  result <- dc(c("sales_q1", "sales_q2", "sales_q3"))
  expect_match(result, "\\{->")
})

test_that("describe_columns - purely numeric first token excluded from prefix grouping", {
  result <- dc(c("1_alpha", "1_beta", "2_gamma"))
  expect_false(grepl("\\{->", result))
})

test_that("describe_columns - purely numeric last token excluded from suffix grouping", {
  result <- dc(c("place_1", "runner_1", "winner_2"))
  expect_false(grepl("\\*_1", result))
})


# ── describe_columns - output format ──────────────────────────────────────────

test_that("describe_columns - returns single character string", {
  result <- dc(c("a_x", "a_y", "b_z"))
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("describe_columns - patterns and residuals are separated by ', '", {
  result <- dc(c("amt_paid", "amt_due", "standalone"))
  parts  <- strsplit(result, ", ")[[1L]]
  expect_gte(length(parts), 2L)
})

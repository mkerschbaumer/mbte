context("nest_signals")

test_input_not_tbl_mbte(mbte_nest_signals)

# all column names in ellipsis (interpreted as symbols) must be present as
# columns
test_that("error for missing specified gropuing variable", {
  tbl <- gen_raw_tbl_mbte()

  # test for specific grouping-related error
  expect_spec_error <- function(...) {
    expect_error(mbte_nest_signals(tbl, ...),
      class = "err_col_expected",
      regexp = "missing group\\w*.*variable"
    )
  }

  # without splicing
  expect_spec_error(nonexistent_colname)
  expect_spec_error("nonexistent_colname")

  # with splicing
  sym <- rlang::sym("nonexistent_colname")
  str <- "nonexistent_colname"
  expect_spec_error(!!sym)
  expect_spec_error(!!str)
})

# delete a specific column and make sure the right error (err_col_expected) gets
# raised
check_nonexisting_column <- function(target, colname_fun, regexp) {
  tbl <- gen_raw_gr_tbl_mbte()

  # Delete specified column and make sure the set column name is referring to a
  # nonexistent column.
  tbl[[target]] <- NULL
  expect_identical(colname_fun(tbl), rlang::sym(target))
  expect_false(target %in% colnames(tbl))

  # error should get raised because target column is not present in dataset
  expect_error(mbte_nest_signals(tbl),
    class = "err_col_expected",
    regexp = regexp
  )
}

test_that("time-column missing", {
  check_nonexisting_column("t", colname_time, "time.*column")
})

test_that("value-column missing", {
  check_nonexisting_column("value", colname_value, "value.*column")
})

# malformat target column and check if the right error (err_class_mismatch) gets
# raised
check_malformatted_column <- function(target, regexp) {
  tbl <- gen_raw_gr_tbl_mbte()

  # make sure `target` is a numeric column in `tbl`
  expect_true(target %in% colnames(tbl))
  expect_true(is.numeric(tbl[[target]]))

  # modify column
  tbl[[target]] <- as.character(tbl[[target]])
  expect_error(mbte_nest_signals(tbl),
    class = "err_class_mismatch",
    regexp = regexp
  )
}

test_that("time column malformatted", {
  check_malformatted_column("t", "not numeric.+time.?column")
})

test_that("value column malformatted", {
  check_malformatted_column("value", "not numeric.+value.?column")
})

# assert grouping columns in ellipsis (if specified) have higher priority than
# preexisting grouping-columns
test_that("columns in ellipsis prioritised", {
  tbl <- gen_ext_tbl_mbte()
  expect_true("extra_column" %in% colnames(tbl))

  # predefined grouping columns should have lower priority than those specified
  # in ellipsis
  tbl <- tbl %>%
    dplyr::group_by(extra_column)

  # create tbl_mbte with custom name for signal-column
  tbl <- new_tbl_mbte(tbl, t, value, signal = sig_col)

  # make sure grouping variable is unchanged
  expect_identical(dplyr::group_vars(tbl), "extra_column")

  # compute expected result for comparison (grouping should be performed on the
  # `mv`-variable because variables in the ellipsis should be priorizized)
  exp <- tbl %>%
    dplyr::group_by(mv, add = FALSE) %>%
    tidyr::nest(t, value, .key = "sig_col") %>%
    mbte_reconstruct(tbl) # needed to keep class attribute "tbl_mbte"

  gr_var <- rlang::sym("mv")
  res <- mbte_nest_signals(tbl, !!gr_var)
  expect_tbl_mbte_equal(res, exp)
})

test_that("expect grouped table if no variables provided in ellipsis", {
  # NOTE: use custom name for signal-column
  tbl <- gen_raw_gr_tbl_mbte() %>%
    new_tbl_mbte(t, value, signal = signal_var)

  # ensure grouping-column "mv" is set
  expect_identical(dplyr::group_vars(tbl), "mv")

  # compute expected result
  exp <- tbl %>%
    tidyr::nest(t, value, .key = signal_var) %>%
    mbte_reconstruct(tbl) # needed to keep class attribute "tbl_mbte"

  # compute actual result and compare it to expected result
  res <- mbte_nest_signals(tbl)
  expect_tbl_mbte_equal(res, exp)
})

# assumption: no grouping columns in ellipsis specified ==> table must be
# grouped
test_that("error when not grouped", {
  tbl <- gen_raw_tbl_mbte()
  expect_false(dplyr::is_grouped_df(tbl))

  expect_error(mbte_nest_signals(tbl), class = "err_expected_grouped")
})

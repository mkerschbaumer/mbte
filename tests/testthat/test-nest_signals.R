context("nest_signals")

test_input_not_tbl_mbte(mbte_nest_signals)

# all column names in ellipsis (interpreted as symbols) must be present as
# columns
test_that("error for missing specified gropuing variable", {
  withr::with_seed(testing_seed(), {
    tbl <- gen_raw_tbl_mbte()
  })

  # test for specific grouping-related error
  expect_spec_error <- function(...) {
    expect_error(mbte_nest_signals(tbl, ...), class = "err_col_expected",
      regexp = "missing group\\w*.*variable")
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

# delete a specific column and make sure the right error gets raised
check_nonexisting_column <- function(target, attr_fun, regexp) {
  # generate grouped raw tbl_mbte
  withr::with_seed(testing_seed(), {
    tbl <- gen_raw_gr_tbl_mbte()
  })
  expect_true(dplyr::is_grouped_df(tbl))
  expect_true(is_tbl_mbte(tbl))

  # delete specified column and make sure the attribute for the target column
  # is referring to a nonexisting column
  tbl[[target]] <- NULL
  expect_identical(attr_fun(tbl), rlang::sym(target))
  expect_false(target %in% colnames(tbl))

  # error should get raised because target column is not present in dataset
  expect_error(mbte_nest_signals(tbl), class = "err_col_expected",
    regexp = regexp)
}

test_that("time-column missing", {
  check_nonexisting_column("time", attr_time, "time.*column")
})

test_that("value-column missing", {
  check_nonexisting_column("value", attr_value, "value.*column")
})

# malformat target column and check if the right error (err_class_mismatch) gets
# raised
check_malformatted_column <- function(target, regexp) {
  withr::with_seed(testing_seed(), {
    tbl <- gen_raw_gr_tbl_mbte()
  })

  # make sure `target` is a numeric column in `tbl`
  expect_true(target %in% colnames(tbl))
  expect_true(is.numeric(tbl[[target]]))

  # modify column
  tbl[[target]] <- as.character(tbl[[target]])
  expect_error(mbte_nest_signals(tbl), class = "err_class_mismatch",
    regexp = regexp)
}

test_that("time column malformatted", {
  check_malformatted_column("time", "not numeric.+time.?column")
})

test_that("value column malformatted", {
  check_malformatted_column("value", "not numeric.+value.?column")
})

# assert grouping columns in ellipsis (if specified) have higher priority than
# preexisting grouping-columns
test_that("columns in ellipsis prioritised", {
  # ensure reproducibility of random dataset generation
  withr::with_seed(testing_seed(), {
    # generate random column-names for time-, value- and signal columns
    time <- gen_random_sym(8L)
    value <- gen_random_sym(8L)
    signal <- gen_random_sym(8L)

    # generate extended raw dataset (contains simulated grouping columns)
    tbl <- gen_ext_raw_dataset(n = 5L)

    # get strings of possible grouping-columns
    possible_gr_cols <- names(purrr::keep(tbl, is.character))

    # predefined grouping columns - see below
    pr_gr_cols <- sample(possible_gr_cols, 2)

    # actual grouping columns - chosen randomly from the remaining ones
    gr_cols <- sample(setdiff(possible_gr_cols, pr_gr_cols), 2)
  })

  # rename time- and value- column to randomly generated column-names
  expect_true(is.numeric(tbl$time)) # ensure presence of column `t`
  expect_true(is.numeric(tbl$value)) # ensure column `value` is present
  tbl <- dplyr::rename(tbl, !!time := time, !!value := value)

  # predefined grouping columns should have lower priority than those specified
  # in ellipsis)
  pr_gr_syms <- rlang::syms(pr_gr_cols)
  tbl <- dplyr::group_by(tbl, !!!pr_gr_syms) # add predefined grouping

  # construct `tbl_mbte`
  tbl <- new_tbl_mbte(tbl, !!time, !!value, signal = !!signal)
  # make sure grouping is unchanged
  expect_identical(dplyr::group_vars(tbl), pr_gr_cols)

  # actual grouping columns for ellisis as symbols - should be prioritised over
  # `pr_gr_cols`
  gr_syms <- rlang::syms(gr_cols)

  # compute expected result for comparison
  exp <- tbl %>%
    dplyr::group_by(!!!gr_syms, add = FALSE) %>%
    tidyr::nest(!!time, !!value, .key = !!signal) %>%
    mbte_reconstruct(tbl) # needed to keep class attribute "tbl_mbte"

  # splice grouping columns as strings
  res <- mbte_nest_signals(tbl, !!!gr_cols)
  expect_tbl_mbte_equal(res, exp)

  # splice grouping columns as symbols
  res <- mbte_nest_signals(tbl, !!!gr_syms)
  expect_tbl_mbte_equal(res, exp)
})

test_that("expect grouping if no variables in ellipsis", {
  withr::with_seed(testing_seed(), {
    time <- gen_random_sym()
    value <- gen_random_sym()
    signal <- gen_random_sym()

    # generate sample raw dataset with additional simulated grouping-columns
    # (character-columns)
    tbl <- gen_ext_raw_dataset(4L)
    expect_true(is.numeric(tbl$time)) # ensure presence of column `time`
    expect_true(is.numeric(tbl$value)) # ensure column `value` is present

    # get possible grouping-columns
    possible_gr_cols <- names(purrr::keep(tbl, is.character))

    # choose 2 grouping columns
    gr_cols <- sample(possible_gr_cols, 2)
    gr_syms <- rlang::syms(gr_cols)
  })

  # rename time- and value columns to randomly generated column names, add
  # grouping and convert to tbl_mbte.
  tbl <- tbl %>%
    dplyr::rename(!!time := time, !!value := value) %>%
    dplyr::group_by(!!!gr_syms, add = FALSE) %>%
    new_tbl_mbte(!!time, !!value, signal = !!signal)

  # ensure grouping-columns are preserved
  expect_identical(dplyr::group_vars(tbl), gr_cols)
  expect_true(is_tbl_mbte(tbl))

  # compute expected result
  exp <- tbl %>%
    tidyr::nest(!!time, !!value, .key = !!signal) %>%
    mbte_reconstruct(tbl) # needed to keep class attribute "tbl_mbte"

  # compute actual result and compare it to expected result
  res <- mbte_nest_signals(tbl)
  expect_tbl_mbte_equal(res, exp)
})

# assumption: no grouping columns in ellipsis specified ==> table must be
# grouped
test_that("error when not grouped", {
  withr::with_seed(testing_seed(), {
    tbl <- gen_raw_tbl_mbte()
  })

  expect_true(is_tbl_mbte(tbl))
  expect_false(dplyr::is_grouped_df(tbl))
  expect_error(mbte_nest_signals(tbl), class = "err_expected_grouped")
})
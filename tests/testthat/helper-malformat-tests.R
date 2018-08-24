# These helpers test if the right errors are raised, if the input is
# malformatted (e.g. a column is missing, wrong type etc.)
#
# Those tests were moved to a separate file, since they are needed to test
# multiple functions, which have the same requirements for the data-input
# parameter.

# NOTE: `fun` is the function to test, which takes the dataset as the first
# input (e.g. mbte_extract_subsignals())
test_input_not_tbl_mbte <- function(fun) {
  test_that("input not `tbl_mbte`", {
    expect_error(fun(NULL), class = "err_not_tbl_mbte")
    expect_error(fun(iris), class = "err_not_tbl_mbte")
    expect_error(fun(tibble::as_tibble(iris)), class = "err_not_tbl_mbte")
  })
}

# The ellipsis for the function to test must be named. The user can provide
# a quosure, where the function to test gets called with at least one unnamed
# parameter. An error inheriting from "err_expected_named" is expected.
#
# e.g. test_ellipsis_unnamed(fun_to_test(dat, unnamed1, unnamed2))
test_ellipsis_unnamed <- function(expr) {
  expr <- rlang::enquo(expr)

  test_that("ellipsis not named", {
    expect_error(!!expr,
      class = "err_expected_named",
      regexp = "elements.+\\.{3}"
    )
  })
}

# Create a testing function to verify that a malformatted or missing list column
# (`target`) gets detected properly.
#
# Assumptions:
# + an error of class "err_col_expected" is raised if the target column is
#   not present in the dataset. `regexp_np` is the corresponding regular
#   expression.
# + "err_class_mismatch" is the error class for the raised error if the target
#   column is malformatted (provide an integer- instead of a list-column). The
#   corresponding regular expression is `regexp_mf`.
create_col_np_mf_tester <- function(target, regexp_np, regexp_mf) {
  # return the actual testing function, which takes the function to test and the
  # dataset to use as arguments
  function(fun, tbl) {
    test_that(paste(target, "column not present or malformatted"), {
      # make sure target column is not present in dataset
      stopifnot(!target %in% colnames(tbl), is_tbl_mbte(tbl), is.function(fun))

      # column not present ==> error expected
      expect_error(fun(tbl), class = "err_col_expected", regexp = regexp_np)

      # add an integer column (is not a list-column ==> error expected)
      tbl[[target]] <- seq_len(nrow(tbl))
      expect_error(fun(tbl), class = "err_class_mismatch", regexp = regexp_mf)
    })
  }
}

# test signal column is not present or malformatted (should be a list)
test_signal_col_np_mf <- create_col_np_mf_tester(
  target = "signal", # name of column (default assumed)
  regexp_np = "signal.+column",
  regexp_mf = "x\\$signal.+not.+list"
)

# test fit column not present or malformatted (fits-column should also be a
# list-column)
test_fits_col_np_mf <- create_col_np_mf_tester(
  target = "fits", # default for fits-column
  regexp_np = "fits.+column",
  regexp_mf = "x\\$fits.+not.+list"
)

# created for testing if `fun` (e.g. mbte_extract_subsignals() detects invalid
# columns in a signal subtibble). The target column of the subtibble must be
# numeric but gets converted to a character on purpose - the error gets
# retrieved from the error log.
test_faulty_signal_subtable <- function(fun, tbl, target_col,
                                        signal = "signal", row_nr = 2L) {
  test_that(paste0(target_col, " column in signal malformatted"), {
    # make sure all signal-subtibbles contain the numeric target column (e.g.
    # numeric time-column if `target_col` is set to "t")
    expect_true(all(purrr::map_lgl(tbl[[signal]], ~{
      is.numeric(.x[[target_col]])
    })), info = "assumed valid signal-column at the beginning")

    # create malformatted signal subtibble at the specified row-number
    malformatted_sig <- tbl[[signal]][[row_nr]]
    malformatted_sig[[target_col]] <- as.character(
      malformatted_sig[[target_col]]
    )
    tbl[[signal]][[row_nr]] <- malformatted_sig

    with_event_log(
      fun(tbl),
      # general integrity check of event-log
      gen_check = ~{
        # make sure the event log only contains 1 row (==> 1 recorded event
        # expected because only one signal-subtibble has been malformatted)
        expect_equal(nrow(.x), 1)

        # make sure the event log contains the right position of the subtibble,
        # which has been malformatted
        expect_equal(.x$row_nr, row_nr)
        expect_equal(.x$signal[[1]], malformatted_sig)
      },
      # the recorded error must be of class "err_class_mismatch" and contains
      # the name of the malformatted column
      err_check = err_class_mismatch_checker(
        paste0("\\w+\\$", target_col, ".+not.+numeric")
      )
    )
  })
}

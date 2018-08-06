# these  helpers test if the right errors are raised, if the input is
# malformatted (e.g. a column is missing, wrong type etc.)
#
# Those tests moved to a separate file, since they are needed to test multiple
# functions, which make the same assumptions about the data-input parameter.

# NOTE: `fun` is the function to test, which takes the dataset as the first
# input (e.g. mbte_extract_subsignals())
test_input_not_tbl_mbte <- function(fun) {
  test_that("input not `tbl_mbte`", {
    expect_error(fun(NULL), class = "err_not_tbl_mbte")
    expect_error(fun(iris), class = "err_not_tbl_mbte")
    expect_error(fun(tibble::as_tibble(iris)), class = "err_not_tbl_mbte")
  })
}

# the ellipsis for the function to test should be named. The user can provide
# a quosure, where this is the case. The correpsonding error
# "err_expected_named" is expected
#
# e.g. test_ellipsis_unnamed(fun_to_test(dat, unnamed1, unnamed2))
test_ellipsis_unnamed <- function(expr) {
  expr <- rlang::enquo(expr)

  test_that("ellipsis not named", {
    expect_error(!!expr, class = "err_expected_named", regexp = "elements.+\\.{3}")
  })
}

# test signal column is not present or malformatted (should be a list)
test_signal_col_np_mf <- function(fun, tbl) {
  test_that("signal column not present or malformatted", {
    stopifnot(!"signal" %in% colnames(tbl), is_tbl_mbte(tbl), is.function(fun))

    # signal column not present
    expect_error(fun(tbl), class = "err_col_expected", regexp = "signal.+column")

    # add a numeric signal column (not a list-column ==> error expected)
    tbl$signal <- seq_len(nrow(tbl))
    expect_error(fun(tbl), class = "err_class_mismatch",
      regexp = "x\\$signal.+not.+list")
  })
}

# created for testing if `fun` (e.g. mbte_extract_subsignals() detects invalid
# columns in a signal subtibble). The target column of the subtibble must be
# numeric but gets converted to a character on purpose - the error gets
# retrieved from the error log.
test_malformatted_signal_subtable <- function(fun, tbl, target_col,
                                           signal = "signal", row_nr = 2L) {
  test_that(paste0(target_col, " column in signal malformatted"), {
    # make sure all signal-subtibbles contain the numeric target column (e.g.
    # numeric time-column if `target_col` is set to "time")
    expect_true(all(purrr::map_lgl(tbl[[signal]], ~{
      is.numeric(.x[[target_col]])
    })))

    # create malformatted signal subtibble at the specified row-number
    malformatted_sig <- tbl[[signal]][[row_nr]]
    malformatted_sig[[target_col]] <- as.character(malformatted_sig[[target_col]])
    tbl[[signal]][[row_nr]] <- malformatted_sig

    with_event_log(
      fun(tbl),
      # general integrity check of the whole event-log
      gen_check = ~{
        # make sure the event log only has 1 row (==> 1 recorded event)
        expect_equal(nrow(.x), 1)

        # make sure the event log contains the right position of the subtibble,
        # which has been malformatted
        expect_equal(.x$row_nr, row_nr)
        expect_equal(.x$signal[[1]], malformatted_sig)
      },
      # the recorded error must be of class "err_class_mismatch"
      err_check = err_class_mismatch_checker(
        paste0("\\w+\\$", target_col, ".+not.+numeric")
      )
    )
  })
}
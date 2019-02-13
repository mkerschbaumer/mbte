context("unnest_fits")

# provide reference implementation of mbte_unnest_fits()
# NOTE: the time and value symbols are quoted.
reference_implementation <- function(x, time_sym, value_sym) {
  time_sym <- rlang::ensym(time_sym)
  value_sym <- rlang::ensym(value_sym)

   x %>%
    dplyr::mutate(fits = map2(signal, fits, ~{
      # add time column to fits and convert from wide- to long form
      tidyr::gather(bind_cols(.x[as.character(time_sym)], .y), "fit",
        !!value_sym, -!!time_sym
      )
    })) %>%
    tidyr::unnest(fits) %>%
    dplyr::select(mv, signal_nr, fit, !!time_sym, !!value_sym) %>%
    mbte_reconstruct(x)
}

# create dataset needed for testing
fitted <- mbte_fit(filtered_signals, lm = lm(value ~ t, .signal))

test_input_not_tbl_mbte(mbte_unnest_fits)

# `signal`-column not present or malformatted
test_signal_col_np_mf(mbte_unnest_fits, gen_raw_tbl_mbte())

# `fits`-column not present or malformatted
test_fits_col_np_mf(mbte_unnest_fits, filtered_signals)

# NOTE: only the time column of a signal is relevant; signal-values not needed
# for mbte_unnest_fits()
test_that("time column not present or malformatted", {
  # signal to malformat
  bad_signal <- fitted$signal[[3]]
  stopifnot("t" %in% colnames(bad_signal)) # make sure time column is present

  # malformat signal on purpose (time expected to be numeric, converted to
  # character)
  bad_signal$t <- as.character(bad_signal$t)
  fitted$signal[[3]] <- bad_signal # override with corrupted signal-tibble

  # expect error describing where the malformatted signal can be found
  expect_error(mbte_unnest_fits(fitted),
    regexp = "x$signal[[3L]]$t",
    class = "err_class_mismatch",
    fixed = TRUE
  )

  # "delete" time column (t)
  bad_signal$t <- NULL
  fitted$signal[[3]] <- bad_signal

  # expect error about column not present
  expect_error(mbte_unnest_fits(fitted),
    regexp = "x$signal[[3L]]",
    class = "err_col_expected",
    fixed = TRUE
  )
})

test_that("don't rely on standard colnames for time and value columns", {
  # construct dataset with modified time and value colnames
  dataset <- raw_signals %>%
    dplyr::rename(signal_time = t, signal_value = value) %>%
    new_tbl_mbte(signal_time, signal_value) %>%
    mbte_nest_signals(mv) %>%
    mbte_extract_subsignals() %>%
    # perform dummy fitting using linear trend module
    mbte_fit(lin = !!tr_linear())

  # compute result for function to test and compare it to the reference
  # implementation.
  res <- expect_silent(mbte_unnest_fits(dataset))
  reference <- reference_implementation(dataset, signal_time, signal_value)
  expect_tbl_mbte_equal(res, reference)
})

# no errors expected
test_that("positive test", {
  # compute expected result
  exp <- reference_implementation(fitted, t, value)
  # compute actual result
  res <- expect_silent(mbte_unnest_fits(fitted))

  # expect matching results
  expect_tbl_mbte_equal(res, exp)
})

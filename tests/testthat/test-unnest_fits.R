context("unnest_fits")

# create datasets needed for testing
raw_tbl_mbte <- gen_raw_tbl_mbte()
fitted <- mbte_fit(filtered_signals, lm = lm(value ~ t, .signal))

test_input_not_tbl_mbte(mbte_unnest_fits)

# signal column not present or malformatted
test_signal_col_np_mf(mbte_unnest_fits, raw_tbl_mbte)

# fits column not present or malformatted
test_fits_col_np_mf(mbte_unnest_fits, filtered_signals)

# NOTE: only the time column of a signal is relevant; signal-values not needed
# for mbte_unnest_fits()
test_that("time column not present or malformatted", {
  bad_signal <- fitted$signal[[3]]
  stopifnot("t" %in% colnames(bad_signal)) # make sure time column is present

  # malformat signal on purpose (time expected to be numeric, converted to
  # character)
  bad_signal$t <- as.character(bad_signal$t)
  fitted$signal[[3]] <- bad_signal

  # expect error describing where the malformatted signal can be found
  expect_error(mbte_unnest_fits(fitted), "x$signal[[3L]]$t",
    "err_class_mismatch", fixed = TRUE)

  # "delete" time column (t)
  bad_signal$t <- NULL
  fitted$signal[[3]] <- bad_signal

  # expect error about column not found
  expect_error(mbte_unnest_fits(fitted), "x$signal[[3L]]", "err_col_expected",
    fixed = TRUE)
})

# no errors expected
test_that("positive test", {
  # expected result
  exp <- fitted %>%
    mutate(fits = map2(signal, fits, ~{
      # add time column to fits and convert from wide form to long
      gather(bind_cols(.y, .x["t"]), "fit", "value", -t)
    })) %>%
    unnest(fits) %>%
    select(mv, signal_nr, fit, t, value) %>%
    mbte_reconstruct(fitted)

  # compute actual result
  res <- mbte_unnest_fits(fitted)

  # expect matching results
  expect_tbl_mbte_equal(res, exp)
})
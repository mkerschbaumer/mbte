context("fitting-helpers")

# positive test for linear trend fitting helper - enusure correctness of
# predicted signal-values
test_that("linear", {
  res <- expect_silent(mbte_fit(filtered_signals, lin = !!tr_linear()))

  expect_tbl_mbte_equal(res, ref_linear)
  # make sure no errors are produced
  expect_null(mbte_event_log(res))
})

# positive test for exponential trend fitting helper (no errors expected)
test_that("exponential", {
  res <- expect_silent(mbte_fit(filtered_signals, exp = !!tr_exponential()))

  expect_tbl_mbte_equal(res, ref_exponential)
  expect_null(mbte_event_log(res))
})

# negative values are contained => currently it is expected, that NA's will be
# returned, since a log transformation is used.
test_that("exponential - negative values", {
  # perform fitting: no errors expected
  fits <- expect_silent(
    mbte_fit(dat_neg_exponential, exp = !!tr_exponential())
  )

  # ensure only NA's are be returned
  expect_equal(nrow(fits), 1L)
  expect_equal(fits$fits[[1]], tibble(exp = rep(NA_real_, 10)))
})

# custom comparison function: hande numeric comparison by testing for near
# equality with a custom tolerance.
comp_fun <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    all(dplyr::near(x, y, tol = 1e-5))
  } else {
    # fallback
    all.equal(x, y)
  }
}

# positive tests for logistic trend (no errors expected)
#
# NOTE: multiple signals are tested (x-shift considered too)
local({
  to_test <- tibble(
    ind = seq_along(dat_logistic), # dataset nr. (needed if a test fails)
    to_fit = dat_logistic,
    ref = ref_logistic
  )

  # loop over created datasets (and corresponding reference)
  purrr::pwalk(to_test, function(ind, to_fit, ref) {
    test_that(paste("logistic trend", ind), {
      # perform fitting using logistic fitting helper
      fits <- suppressWarnings(mbte_fit(to_fit, log = !!tr_logistic()))

      # make sure produced fits match reference
      expect_tbl_mbte_equal(fits, ref, comp_fun = comp_fun)
      # no events/errors expected
      expect_null(mbte_event_log(fits))
    })
  })
})

# expect that fitting fails (e.g. in this case a sine-like signal is provided)
test_that("logistic - no convergence", {
  # No warning/output regarding event log expected.
  fits <- expect_silent(
    mbte_fit(dat_convergence_failure, log = !!tr_logistic())
  )

  # make sure NA's are produced
  expect_equal(nrow(fits), 1L)
  expect_equal(fits$fits[[1]], tibble(log = rep(NA_real_, 30)))

  # check event log (should be empry)
  event_log <- mbte_event_log(fits)
  expect_null(event_log)
})

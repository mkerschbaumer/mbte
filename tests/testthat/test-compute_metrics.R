context("compute_metrics")

# generate global table needed for the following tests (to avoid generating
# it mulitple times)

# dataset with fitted signals; NOTE: make sure mbte_compute_fits() doesen't rely
# on default names for `signals` and `fits`-columns by providing custom column
# names
fitted <- filtered_signals %>%
  dplyr::rename(signal_var = signal) %>%
  new_tbl_mbte(t, value, signal = signal_var, fits = fits_var) %>%
  mbte_fit(loess = loess(value ~ t, .signal))

# expected result if a metric computation fails (by definition, a failed metric
# computation should result in `NA_real_`); NOTE: "faulty_metric" is the name of
# the faulty metric quosure.
exp_all_NA <- fitted %>%
  gen_metric_result(NA_real_, "faulty_metric", signal_var, fits_var)

# create an event log-checker (check overall integrity of event log) - used for
# performing general checks of the event log.
#
# assumption: the `fitted`-dataset has been used to compute the metrics
create_el_checker <- function(exp_fit_name, exp_metric_name, exp_metric_quo) {
  function(event_log) {
    stopifnot(tibble::is_tibble(event_log))

    event_log %>%
      {
        list(.$row_nr, .$fit_name, .$metric_name, .$metric_quo, .$pred, .$obs)
      } %>%
      purrr::pwalk(function(row, fit_name, metric_name, metric_quo, pred, obs) {
        info <- paste("row of mismatch:", row)

        # create known values for checking equality (expected values for
        # observed- and predicted signal-values)
        exp_obs <- fitted$signal_var[[row]]$value
        exp_pred <- fitted$fits_var[[row]][[fit_name]]

        # check equality of fit_name, metric_name, metric_quo, predicted
        # signal-values and original signal-values (observed signal-values)
        expect_equal(fit_name, exp_fit_name, info = info)
        expect_equal(metric_name, exp_metric_name, info = info)
        expect_equal(metric_quo, exp_metric_quo, info = info)
        expect_equal(pred, exp_pred, info = info)
        expect_equal(obs, exp_obs, info = info)
      })
  }
}

test_input_not_tbl_mbte(mbte_compute_metrics)

test_ellipsis_unnamed(mbte_compute_metrics(fitted, 25))

# test signal column not present or malformatted
test_signal_col_np_mf(mbte_compute_metrics, gen_raw_tbl_mbte())

test_that("evaluation error metric quosure", {
  errmsg <- "error gets raised by metric quosure"
  metric_quo <- rlang::quo(stop(errmsg))

  res <- with_event_log(
    mbte_compute_metrics(fitted, faulty_metric = !!metric_quo),
    err_check = err_eval_metric_checker(errmsg),
    gen_check = create_el_checker("loess", "faulty_metric", metric_quo)
  )

  expect_equal(res, exp_all_NA)
})

# Helper function to test cases, where the evaluation of a metric quosure
# doesen't result in a scalar numeric
check_not_scalar_numeric <- function(metric_quo) {
  metric_quo <- rlang::enquo(metric_quo) # quote input

  res <- with_event_log(
    mbte_compute_metrics(fitted, faulty_metric = !!metric_quo),
    err_check = err_class_mismatch_checker(
      errmsg = "not.+scalar.+numeric.+result.+metric.+quosure"
    ),
    gen_check = create_el_checker("loess", "faulty_metric", metric_quo)
  )

  # no actual metrics should have been computed ==> only NA's, since result
  # generated by metric quosure should have been detected as faulty
  expect_equal(res, exp_all_NA)
}

test_that("result not scalar numeric - character", {
  check_not_scalar_numeric("invalid")
})

test_that("result not scalar numeric - wrong length", {
  check_not_scalar_numeric(1:10)
})

# allow metric-quosures to return integers as results
test_that("integer-result allowed", {
  metric_quo <- rlang::quo(42L)

  # no errors/warnings expected
  res <- expect_silent(mbte_compute_metrics(fitted, int_quo = !!metric_quo))

  # event-log should be empty
  expect_null(mbte_event_log(res))

  # generate expected result (metric quosure only returned 42)
  exp <- gen_metric_result(fitted, 42L, "int_quo", signal_var, fits_var)

  # make sure results match
  expect_deep_equal(res, exp)
})

# no errors expected
test_that("positive test", {
  # normalized root mean squared error, arbitrary error metric
  nrmse <- function(pred, obs) {
    sqrt(mean((pred - obs)^2)) / (max(obs) - min(obs))
  }

  # list used for tidy-dots splicing
  metrics <- list(nrmse = rlang::quo(nrmse(.pred, .obs)))

  # compute expected result
  exp <- fitted %>%
    dplyr::mutate(.tmp = purrr::map2(signal_var, fits_var, ~{
      obs <- .x$value # original signal-values

      # iterate over `fits`-tibbles (`.y` is a tibble)
      purrr::imap_dfr(.y, ~{
        pred <- .x # predicted signal-values
        fit_name <- .y # e.g. "loess"
        purrr::imap_dfr(metrics, ~{
          # NOTE: `.x` is a metric quosure and `.y` denotes its name;
          # compute result via metric-quosure (use masking)
          result <- rlang::eval_tidy(.x, list(.pred = pred, .obs = obs))

          list(fit = fit_name, metric = .y, result = result)
        })
      })
    })) %>%
    # remove unneeded columns and unnest temporary column with tibbles
    dplyr::select(-signal_var, -fits_var) %>%
    tidyr::unnest(.tmp)

  # compute actual result (no errors/warnings expected)
  res <- expect_silent(mbte_compute_metrics(fitted, !!!metrics))

  # expect matching results
  expect_deep_equal(res, exp)
})

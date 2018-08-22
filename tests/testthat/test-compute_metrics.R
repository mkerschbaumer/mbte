context("compute_metrics")

# generate global tables needed for the following tests (to avoid generating
# them mulitple times)
withr::with_seed(testing_seed(), {
  # raw dataset (grouped)
  raw_tbl <- gen_raw_gr_tbl_mbte()
  # dataset with signals fitted (loess() just used as an example)
  fitted <- raw_tbl %>%
    mbte_nest_signals() %>%
    mbte_extract_subsignals() %>%
    dplyr::filter(purrr::map_int(signal, nrow) > 20) %>%
    mbte_reconstruct(raw_tbl) %>%
    mbte_fit(loess = loess(value ~ t, .signal))
})

# expected result if the metric computation fails (by definition, a failed
# metric computation should result in `NA_real_`)
exp_all_NA <- local({
  # "faulty_metric" is used as a dummy name for a metric, which evaluation will
  # not result in a scalar numeric
  metric_names <- "faulty_metric"

  fitted %>%
    mutate(.tmp = purrr::map(fits, ~{
      fit_names <- colnames(.x)
      purrr::cross_df(list(fit = fit_names, metric = metric_names,
        result = NA_real_))
    })) %>%
    dplyr::select(-signal, -fits) %>%
    tidyr::unnest(.tmp)
})

# create an event log-checker (check overall consistency of event log) - used
# for general checks of the event log
#
# assumption: the `fitted`-dataset has been used to compute the metrics
create_el_checker <- function(fit_name, metric_name, metric_quo) {
  function(event_log) {
    stopifnot(tibble::is_tibble(event_log))

    event_log %>%
      dplyr::select(-error) %>%
      { list(.$row_nr, .$fit_name, .$metric_name, .$metric_quo, .$pred, .$obs) } %>%
      purrr::pwalk(function(row, fit_n, metric_n, metric_q, pred, obs) {
        info <- paste("row of mismatch:", row)

        # create known values for checking equality (expected values for
        # observed signal-values and predicted signal-values)
        exp_obs <- fitted$signal[[row]]$value
        exp_pred <- fitted$fits[[row]][[fit_n]]

        # check equality of fit_name, metric_name, metric_quo, predicted
        # signal-values and original signal-values (observed signal-values)
        expect_equal(fit_n, fit_name, info = info)
        expect_equal(metric_n, metric_name, info = info)
        expect_equal(metric_q, metric_quo, info = info)
        expect_equal(pred, exp_pred, info = info)
        expect_equal(obs, exp_obs, info = info)
      })
  }
}

test_input_not_tbl_mbte(mbte_compute_metrics)

test_ellipsis_unnamed(mbte_compute_metrics(fitted, 25))

# wraps mbte_compute_metrics() and fill in a valid metric (to avoid errors
# if no metric-quosures are specified)
wrapper <- function(...) {
  mbte_compute_metrics(..., dummy_metric = 25)
}

# test signal column not present or malformatted
test_signal_col_np_mf(wrapper, raw_tbl)

test_that("evaluation error metric quosure", {
  withr::with_seed(testing_seed(), {
    errmsg <- gen_random_string(10L)
    metric_quo <- rlang::quo(stop(errmsg))
  })

  res <- with_event_log(
    mbte_compute_metrics(fitted, faulty_metric = !!metric_quo),
    err_check = err_eval_metric_checker(errmsg),
    gen_check = create_el_checker("loess", "faulty_metric", metric_quo)
  )

  expect_equal(res, exp_all_NA)
})


# create an error checker (assert than an error in the event log is of
# class "err_class_mismatch" and that it has the right error message)
err_checker_scalar_numeric <- create_err_checker(class = "err_class_mismatch",
  errmsg = "not.+scalar.+numeric.+result.+metric.+quosure")

# test cases, where the evaluation of a metric quosure doesen't result in a
# scalar numeric
local({
  faulty_inputs <- list(
    1:10,  # numeric, but of length 10
    "test"
  )

  purrr::iwalk(faulty_inputs, ~test_that(paste("result not scalar numeric", .y), {
    .x <- rlang::enquo(.x) # convert to quosure
    res <- with_event_log(
      mbte_compute_metrics(fitted, faulty_metric = !!.x),
      err_check = err_checker_scalar_numeric,
      gen_check = create_el_checker("loess", "faulty_metric", .x)
    )

    # no actual metrics should have been computed ==> only NA's, since all
    # metric quosures should have been detected as faulty
    expect_equal(res, exp_all_NA)
  }))
})

# allow metric-quosures to return integers as results
test_that("integer-result allowed", {
  # generate random integer, which should be returned by the metric quosure
  withr::with_seed(testing_seed(), {
    int_value <- sample(1L:100L, 1)
    metric_quo <- rlang::quo(!!int_value)
  })

  # no errors/warnings expected
  expect_silent(res <- mbte_compute_metrics(fitted, int_quo = !!metric_quo))
  # event-log should be empty
  expect_null(mbte_event_log(res))

  # compute expected result (simulate a case, where every evaluation of the only
  # metric quosure yields `int_value`)
  exp <- fitted %>%
    # only keep relevant columns
    select(mv, signal_nr, fits) %>%
    mutate(
      # only keep names of fits used to generate `fitted`
      fit = map(fits, colnames),
      fits = NULL, # delete fits-column
      result = int_value,
      metric = "int_quo"
    ) %>%
    unnest(fit) %>%
    select(mv, signal_nr, fit, metric, result) # change column order

  # make sure results match
  expect_deep_equal(res, exp)
})

# no errors expected
test_that("positive test", {
  # normalized root mean squared error, arbitrary error metric
  nrmse <- function(pred, obs) {
    sqrt(mean((pred - obs)^2)) / (max(obs) - min(obs))
  }

  # create a list of metrics (to test splicing support)
  metrics <- list(nrmse = rlang::quo(nrmse(.pred, .obs)))

  # compute expected result
  exp <- fitted %>%
    dplyr::mutate(.tmp = purrr::map2(signal, fits, ~{
      obs <- .x$value
      # iterate over fits (`.y` is a tibble)
      purrr::imap_dfr(.y, ~{
        pred <- .x
        fit_name <- .y
        purrr::imap_dfr(metrics, ~{
          # NOTE: `.x` is a metric quosure, `.y` its name
          # compute result via metric-quosure (also use data-mask)
          result <- rlang::eval_tidy(.x, list(.pred = pred, .obs = obs))
          list(fit = fit_name, metric = .y, result = result)
        })
      })
    })) %>%
    # remove unneeded columns and unnest temporary column with tibbles
    dplyr::select(-signal, -fits) %>%
    tidyr::unnest(.tmp)

  # actual computed result
  res <- mbte_compute_metrics(fitted, !!!metrics)

  # expect matching results
  expect_deep_equal(res, exp)
})

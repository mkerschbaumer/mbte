context("fit")

test_input_not_tbl_mbte(mbte_fit)

test_ellipsis_unnamed(mbte_fit(filtered_signals, loess(value ~ t, .signal)))

# provide a dumming fitting quosure; this wrapper function is only needed
# to test the detection of faulty signal-subtibbles.
fit_wrapper <- function(...) {
  mbte_fit(..., test_fit_quo = .signal[[.value_sym]])
}


# test signal column not present or malformatted
test_signal_col_np_mf(fit_wrapper, gen_raw_tbl_mbte())

test_faulty_signal_subtable(fit_wrapper, filtered_signals, "t")

test_faulty_signal_subtable(fit_wrapper, filtered_signals, "value")

# A helper function for creating a checking-function, that tests if the event
# log produced by mbte_fit() is valid.
#
# + contained signals match signal-subtibbles of the original table at the
#   respective row-number (tested via expect_deep_equal())
# + name of the fitting-quosure, which caused the event, is equal to
#   `fit_name_chr`.
# + fitting quosure, which caused the event, is equal to `fit_quo`.
#
# Assumption: the `filtered_signals`-dataset is used for fitting and 1 error
# gets raised for every processed row.
create_el_checker <- function(fit_name_chr, fit_quo) {
  function(event_log) {
    stopifnot(identical(nrow(event_log), nrow(filtered_signals)))

    event_log %>%
      dplyr::select(row_nr, signal, fit_name, fit_quo) %>%
      {
        list(.$row_nr, .$signal, .$fit_name, .$fit_quo)
      } %>%
      purrr::pwalk(function(row, sig, name, quo) {
        info <- paste("row of mismatch:", row)

        # extract original signal (to ensure the signal of the event and the
        # actual signal, where the event should have occurred, match up)
        orig_signal <- filtered_signals$signal[[row]]
        expect_deep_equal(sig, orig_signal, info)

        # make sure the name of the fitting quosure and the fitting quosure
        # itself match the expected results
        expect_equal(name, fit_name_chr, info = info)
        expect_equal(quo, fit_quo, info = info)
      })
  }
}

# Check the result of a fitting computation, which is expected to produce NA's
# (if an error gets raised or NA's returned). The names of the faulty fitting
# quosures must be equal to `fit_names_chr`.
check_fitting_tables <- function(fits, fit_names_chr) {
  stopifnot(is.list(fits))
  purrr::iwalk(fits, ~{
    info <- paste("fit table at row nr", .y)
    expect_equal(colnames(.x), fit_names_chr, info = info)
    expect_true(all(is.na(.x)), info = info)
  })
}

test_that("fitting quosure evaluation error", {
  errmsg <- "custom error message"
  fit_quo <- rlang::quo(stop(errmsg))

  res <- with_event_log(
    mbte_fit(filtered_signals, fit_name = !!fit_quo),
    # general check; NOTE: it is assumed, that `filtered_signals` contains less
    # than 50 rows, since the event log may have a set limit of recorded events.
    gen_check = create_el_checker("fit_name", fit_quo),
    err_check = err_fit_checker(errmsg)
  )

  # make sure fits-tibbles have the right colnames and only contain NA's
  check_fitting_tables(res$fits, "fit_name")
})

test_that("error during prediction", {
  errmsg <- "custom error message for error during prediction"

  # fitting quosure
  fit_quo <- rlang::quo(lm(value ~ t, .signal))
  res <- with_mock(
    # mock predict.lm to always raise an error (with error message from above)
    predict.lm = function(x, ...) {
      stop(errmsg)
    },
    with_event_log(
      mbte_fit(filtered_signals, fit_name = !!fit_quo),
      gen_check = create_el_checker("fit_name", fit_quo),
      err_check = err_fit_checker(errmsg)
    )
  )

  # make sure colnames match `fit_name_chr` and assert only NA's are produced
  check_fitting_tables(res$fits, "fit_name")
})

test_that("predictions not numeric", {
  # fitting quosure
  fit_quo <- rlang::quo(lm(value ~ t, .signal))
  res <- with_mock(
    # mock predict.lm to always return a character-vector of the correct length
    # instead of a numeric vector
    predict.lm = function(x, newdata, ...) {
      stopifnot(tibble::is_tibble(newdata))
      rep("abc", nrow(newdata))
    },
    with_event_log(
      mbte_fit(filtered_signals, fit_name = !!fit_quo),
      gen_check = create_el_checker("fit_name", fit_quo),
      err_check = err_class_mismatch_checker("fit.+not.+numeric")
    )
  )

  # make sure NA's are produced
  check_fitting_tables(res$fits, "fit_name")
})

test_that("predictions length mismatch", {
  # create a fitting quosure, that always returns a numeric vector with an
  # incompatible length
  fit_quo <- rlang::quo(rep(1, nrow(.signal) + 1))

  res <- with_event_log(
    mbte_fit(filtered_signals, fit_name = !!fit_quo),
    gen_check = create_el_checker("fit_name", fit_quo),
    err_check = err_dim_incomp_checker("[Ii]ncompatible.+fit.+length")
  )

  # make sure only NA's are produced
  check_fitting_tables(res$fits, "fit_name")
})

test_that("positive test", {
  # "fits_var" should be the name of the generated fits-list-column
  filtered_signals2 <- new_tbl_mbte(filtered_signals, t, value, fits = fits_var)

  # create a formula for fitting via loess()
  fit_formula <- value ~ t

  # compute expected result
  exp <- filtered_signals2 %>%
    dplyr::mutate(fits_var = purrr::map(signal, ~{
      fit_values <- predict(loess(fit_formula, .x), newdata = .x)
      stopifnot(is.numeric(fit_values))
      tibble::tibble(fit1 = fit_values, fit2 = fit_values)
    })) %>%
    mbte_reconstruct(filtered_signals2)

  # ensure both ways of specifying a fitting quosure work (returning an object,
  # on which predict() will be called on or returning a numeric vector of the
  # correct length); no errors/warnings expected
  res <- expect_silent(
    mbte_fit(filtered_signals2,
      fit1 = loess(fit_formula, .signal),
      fit2 = predict(loess(fit_formula, .signal), newdata = .signal)
    )
  )

  # make sure no event log has been added
  expect_null(mbte_event_log(res))

  # create custom comparison function (all.equal with checking of names
  # disabled)
  comp_fun <- purrr::partial(all.equal, check.names = FALSE)

  # compare result with expected result (using custom comparison function)
  expect_tbl_mbte_equal(res, exp, comp_fun = comp_fun)
})

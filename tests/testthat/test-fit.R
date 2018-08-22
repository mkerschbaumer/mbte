context("fit")

# global variables generated randomly (to avoid having to recompute them more
# than needed)
withr::with_seed(testing_seed(), {
  # raw table (grouped, no signal extraction or nesting performed)
  raw_tbl <- gen_raw_gr_tbl_mbte()
  # subsignals extracted
  extracted <- raw_tbl %>%
    mbte_nest_signals() %>%
    mbte_extract_subsignals()
  # only keep subsignals with a length greater than 20
  filtered <- extracted %>%
    dplyr::filter(purrr::map_int(signal, nrow) > 20) %>%
    mbte_reconstruct(extracted)

  # generate random name for the name of the fit (name in ellipsis when passed
  # to mbte_fit())
  fit_name_chr <- gen_random_string(5L)
  fit_name_sym <- rlang::sym(fit_name_chr)
})

test_input_not_tbl_mbte(mbte_fit)

test_ellipsis_unnamed(mbte_fit(filtered, loess(value ~ t, .signal)))

# specify a dumming fitting quosure, since this wrapper function is only needed
# to test for detection of malformatted signal-subtibbles.
fit_wrapper <- function(...) {
  mbte_fit(..., test_fit_quo = .signal[[.value_sym]])
}


# test signal column not present or malformatted
test_signal_col_np_mf(fit_wrapper, raw_tbl)

test_malformatted_signal_subtable(fit_wrapper, filtered, "t")

test_malformatted_signal_subtable(fit_wrapper, filtered, "value")

# a helper function for creating a checking-function, that tests if the event
# log produced by mbte_fit() is consistent
#
# + contained signals matching with the signal-subtibble of the original table
#   at the specified row-number (via expect_deep_equal())
# + name of the fitting-quosure, which caused the event, equal to `fit_name_chr`
# + fitting quosure, which caused the event, equal to `fit_quo`.
#
# assumption: the `filtered`-dataset gets used for fitting and 1 error gets
# raised for every processed row
create_el_checker <- function(fit_name_chr, fit_quo) {
  function(event_log) {
    stopifnot(identical(nrow(event_log), nrow(filtered)))

    event_log %>%
      dplyr::select(row_nr, signal, fit_name, fit_quo) %>%
      { list(.$row_nr, .$signal, .$fit_name, .$fit_quo) } %>%
      purrr::pwalk(function(row, sig, name, quo) {
        info <- paste("row of mismatch:", row)

        # extract original signal (to ensure the signal of the event and the
        # actual signal, where the event should have occurred, match up)
        orig_signal <- filtered$signal[[row]]
        expect_deep_equal(sig, orig_signal, info)

        # make sure the name of the fitting quosure and the fitting quosure itself
        # match the expected results
        expect_equal(name, fit_name_chr, info = info)
        expect_equal(quo, fit_quo, info = info)
      })
  }
}

# check the result of a fitting computation (name equal to `fit_name_chr`),
# which is expected to produce NA's (by raising an error or by returning NA's)
check_fitting_tables <- function(fits, fit_name_chr) {
  stopifnot(is.list(fits))
  purrr::iwalk(fits, ~{
    info <- paste("fit table at row nr", .y)
    expect_equal(colnames(.x), fit_name_chr, info = info)
    expect_true(all(is.na(.x)), info = info)
  })
}

test_that("fitting quosure evaluation error", {
  withr::with_seed(testing_seed(), {
    # generate random error message and create quosure, which raises an error
    # with the specified error message in any case, if it gets evaluated
    errmsg <- gen_random_string(10L)
    fit_quo <- rlang::quo(stop(errmsg))
  })

  res <- with_event_log(
    mbte_fit(filtered, !!fit_name_sym := !!fit_quo),
    # general check; (NOTE: it is assumed, that `filtered` contains less than
    # 50 elements, since the event log may have a set limit of recorded events)
    gen_check = create_el_checker(fit_name_chr, fit_quo),
    err_check = err_fit_checker(errmsg)
  )

  # make sure fitting tables have the right colnames and are filled with NA's
  check_fitting_tables(res$fits, fit_name_chr)
})

test_that("error during prediction", {
  withr::with_seed(testing_seed(), {
    # generate random error message for prediction error
    errmsg <- gen_random_string(10L)
  })

  # fitting quosure
  fit_quo <- rlang::quo(lm(value ~ t, .signal))
  res <- with_mock(
    # mock predict.lm to always raise an error (with error message from above)
    predict.lm = function(x, ...) {
      stop(errmsg)
    },
    with_event_log(
      mbte_fit(filtered, !!fit_name_sym := !!fit_quo),
      gen_check = create_el_checker(fit_name_chr, fit_quo),
      err_check = err_fit_checker(errmsg)
    )
  )

  # make sure colnames match `fit_name_chr` and assert only NA's are produced
  check_fitting_tables(res$fits, fit_name_chr)
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
      mbte_fit(filtered, !!fit_name_sym := !!fit_quo),
      gen_check = create_el_checker(fit_name_chr, fit_quo),
      err_check = err_class_mismatch_checker("fit.+not.+numeric")
    )
  )

  # make sure NA's are produced
  check_fitting_tables(res$fits, fit_name_chr)
})

test_that("predictions length mismatch", {
  # create a fitting quosure, that always returns a numeric vector with an
  # incompatible length
  fit_quo <- rlang::quo(rep(1, nrow(.signal) + 1))

  res <- with_event_log(
    mbte_fit(filtered, !!fit_name_sym := !!fit_quo),
    gen_check = create_el_checker(fit_name_chr, fit_quo),
    err_check = err_dim_incomp_checker("[Ii]ncompatible.+fit.+length")
  )

  # make sure only NA's are produced
  check_fitting_tables(res$fits, fit_name_chr)
})

test_that("positive test", {
  withr::with_seed(testing_seed(), {
    # generate random column names
    time <- gen_random_sym()
    value <- gen_random_sym()
    signal <- gen_random_sym()
    fits <- gen_random_sym()
  })

  # rename column names to ensure randomized testing
  filtered <- filtered %>%
    # rename time- and value colnames for signal-subtibbles
    dplyr::mutate(signal = map(signal, ~{
      dplyr::rename(.x, !!time := t, !!value := value)
    })) %>%
    dplyr::rename(!!signal := signal) %>%
    new_tbl_mbte(!!time, !!value, signal = !!signal, fits = !!fits)

  # create a formula for fitting via loess()
  fit_formula <- eval(rlang::expr(!!value ~ !!time))
  stopifnot(rlang::is_formula(fit_formula))

  # compute expected result
  exp <- filtered %>%
    dplyr::mutate(!!fits := purrr::map(!!signal, ~{
      fit_values <- predict(loess(fit_formula, .x))
      stopifnot(is.numeric(fit_values))
      tibble(fit1 = fit_values, fit2 = fit_values)
    })) %>%
    mbte_reconstruct(filtered)

  # ensure both ways of specifying a fitting quosure work (returning an object,
  # on which predict() will be called on or returning a numeric vector of the
  # correct length)
  res <- filtered %>%
    mbte_fit(
      fit1 = loess(fit_formula, .signal),
      fit2 = predict(loess(fit_formula, .signal), newdata = .signal)
    )

  # make sure no event log has been added
  expect_identical(attr_event_log(res), NULL)

  # create custom comparison function (all.equal with names-checking disabled)
  comp_fun <- purrr::partial(all.equal, check.names = FALSE)

  # compare result with expected result (using custom comparison function)
  expect_tbl_mbte_equal(res, exp, comp_fun = comp_fun)
})

context("new_tbl_mbte")

# test if the created object (`obj`) matches the following expectations:
#
# + a valid `tbl_mbte`-object must have been generated (checked via is_tbl_mbte())
# + *time*, *value*, *signal* and *fits* get quoted and used as symbols.
#   `obj` must provide these attributes (which have to be symbols).
# + `subclass` should be a character vector of the subclasses `obj` must inherit
#   from
# + `additional_attrs` is a named list with the additional attributes, that
#   should be set in the call to new_tbl_mbte()
check_new_tbl_mbte <- function(obj, time, value, signal = signal, fits = fits) {
  time <- rlang::ensym(time)
  value <- rlang::ensym(value)
  signal <- rlang::ensym(signal)
  fits <- rlang::ensym(fits)

  # generate addtional information for displaying purposes (if a test fails,
  # this information gets displayed too)
  info <- sprintf(
    "time: %s, value: %s, signal: %s, fits: %s", rlang::expr_label(time),
    rlang::expr_label(value), rlang::expr_label(signal), rlang::expr_label(fits)
  )

  # created object must be recognized as a valid `tbl_mbte`
  expect_true(is_tbl_mbte(obj), info = info)
  expect_true(is_tibble(obj), info = info)

  # relevant attributes must match expectated values
  expect_identical(attr_time(obj), time, info = info)
  expect_identical(attr_value(obj), value, info = info)
  expect_identical(attr_signal(obj), signal, info = info)
  expect_identical(attr_fits(obj), fits, info = info)
}

test_that("creation using tidy semantics", {
  # symbols to pass using tidy semantics
  time <- rlang::sym("time_var")
  value <- rlang::sym("value_var")
  signal <- rlang::sym("signal_var")
  fits <- rlang::sym("fits_var")

  # create new tbl_mbte; NOTE: it is not important if the specified column
  # names are actually present in the dataset
  tbl <- new_tbl_mbte(raw_signals, !!time, !!value,
    signal = !!signal, fits = !!fits
  )

  # check integrity of the generated tbl_mbte
  check_new_tbl_mbte(tbl, !!time, !!value, !!signal, !!fits)
})

test_that("assume quotation - data.frame with defaults", {
  # convert to data.frame on purpose to ensure, that the conversion to a tibble
  # gets performed
  raw_signals_df <- as.data.frame(raw_signals)

  # column-name related inputs should get quoted
  tbl <- new_tbl_mbte(raw_signals_df, time_var, value_var)

  # NOTE: assume defaults for `signal` and `fits`
  check_new_tbl_mbte(tbl, time_var, value_var)
})

test_that("pass strings - defaults", {
  tbl <- new_tbl_mbte(raw_signals, "time_var", "value_var")

  # NOTE: defaults expected for `signal` and `fits`-attribute
  check_new_tbl_mbte(tbl, time_var, value_var)
})

test_that("inherits from subclasses", {
  # subclasses to inherit from
  subclasses <- c("custom_subclass1", "custom_subclass2")

  tbl <- new_tbl_mbte(raw_signals, time, value, subclass = subclasses)

  # perform general checks
  check_new_tbl_mbte(tbl, time, value)

  # make sure generated tbl_mbte inherits from the specified subclasses
  expect_is(tbl, "custom_subclass1")
  expect_is(tbl, "custom_subclass2")
})

test_that("error wrong type for subclasses", {
  # subclasses must be of type character
  expect_error(
    new_tbl_mbte(raw_signals, time, value, subclass = c(1, 2)),
    class = "err_class_mismatch", regexp = "subclass"
  )
})

# attributes should be set correctly
test_that("attributes are set", {
  tbl <- new_tbl_mbte(raw_signals, time, value, custom_attribute = 42)

  # perform general checks
  check_new_tbl_mbte(tbl, time, value)
  # make sure `custom_attribute` has been set
  expect_equal(attr(tbl, "custom_attribute"), 42)
})

test_that("error if not all elements of ellipsis named", {
  # NOTE: `50` is unnamed and should trigger an error
  expect_error(
    new_tbl_mbte(raw_signals, time, value, a1 = 10, 50),
    class = "err_expected_named", regexp = "\\.{3}.+named"
  )
})

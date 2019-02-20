context("colnames")

# NOTE: Currently it is not assumed, that `x` is a tbl_mbte.

test_getter <- function(f_getter, name, x = filtered_signals) {
  # Currenlty the name is determined by looking up the attribute (expected to
  # be a symbol).
  expect_identical(f_getter(x), attr(x, name, exact = TRUE))
  expect_true(is_symbol(f_getter(x)))

  # Override attribute and expect, that NULL is returned.
  attr(x, name) <- NULL
  expect_null(f_getter(x))

  # Ensure, that no partial matching is performed
  attr(x, paste0(name, "abc")) <- sym("test")
  expect_null(f_getter(x), "unexpected partial matching")
}

test_setter <- function(f_setter) {
  x <- filtered_signals

  # Make sure check for input being a symbol is implemented.
  expect_error(f_setter(x, 12L), "symbol")
  expect_error(f_setter(x, "abc"), "symbol")
  expect_error(f_setter(x, NULL), "symbol")
  expect_error(f_setter(x, list()), "symbol")

  # correct input (no errors expected)
  expect_silent(f_setter(x, rlang::sym("abc")))
}

test_that("colname_set() parameters", {
  x <- filtered_signals

  # Ensure name parameter is checked (string expected)
  expect_error(colname_set(x, sym("abc"), 12L), "character")
  expect_error(colname_set(x, sym("abc"), letters), "length")

  # Use testing function to test faulty inputs (errors expected) and perform
  # positive test.
  test_setter(function(...) {
    colname_set(..., name = "dummy_colname")
  })
})

test_that("colname_get() parameters", {
  x <- filtered_signals

  # Ensure name parameter is checked properly (string expected)
  expect_error(colname_get(x, 12L), "character")
  expect_error(colname_get(x, letters), "length")

  # Extract dummy column name
  attr(x, "dummy_colname") <- sym("test")

  # Use testing function to test faulty inputs (errors expected) and perform
  # positive test.
  test_getter(function(...) {
    colname_get(..., name = "dummy_colname")
  }, name = "dummy_colname", x = x)
})

# Ensure colname_get() + colname_set() are compatible.
test_that("colname_get() + colname_set()", {
  x <- filtered_signals
  colname <- "dummy_colname"
  to_set <- sym("dummy_value") # symbol/colname to set

  # Ensure no column name is set.
  expect_null(colname_get(x, colname), "No initial set column name expected")
  # Override column nmae.
  x <- expect_silent(colname_set(x, to_set, colname))
  # Make sure column name has been set properly.
  expect_identical(colname_get(x, colname), to_set, "column name not set properly")
})

test_that("time colname", {
  test_getter(colname_time, "time")
  test_setter(`colname_time<-`)
})


test_that("value colname", {
  test_getter(colname_value, "value")
  test_setter(`colname_value<-`)
})

test_that("signal colname", {
  test_getter(colname_signal, "signal")
  test_setter(`colname_signal<-`)
})

test_that("fits colname", {
  test_getter(colname_fits, "fits")
  test_setter(`colname_fits<-`)
})


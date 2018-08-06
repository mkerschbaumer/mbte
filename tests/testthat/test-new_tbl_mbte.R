context("new_tbl_mbte")

# NOTE: variable-related parameters of new_tbl_mbte() like `signal` get tested
# by providing strings and symbols (via tidy semantics).

# test if the created object (`obj`) matches the following expectations:
#
# + a valid `tbl_mbte`-object must have been generated (checked via is_tbl_mbte())
# + *time*, *value*, *signal*, *fits* and *metric* get quoted and used
#   as symbols. `obj` must provide these attributes (which have to be symbols).
# + `subclass` should be a character vector of the subclasses `obj` must inherit
#   from
# + `additional_attrs` is a named list with the additional attributes, that
#   should be set in the call to new_tbl_mbte()
#
# NOTE: ds_name denotes the name of the dataset (as string) - used for
# displaying purposes when an expectation fails
test_new_tbl_mbte <- function(obj, ds_name, time, value, signal,
                              fits, metric, subclass = NULL,
                              additional_attrs = NULL) {
  time <- ensym(time)
  value <- ensym(value)
  signal <- ensym(signal)
  fits <- ensym(fits)
  metric <- ensym(metric)

  # generate addtional information for displaying purposes (if a test fails,
  # this information gets displayed too)
  info <- sprintf(paste("dataset_name: '%s', time: %s, value: %s, signal: %s,",
    "fits: %s, metric: %s, additional_attrs: %s"), ds_name,
    expr_label(time), expr_label(value), expr_label(signal), expr_label(fits),
    expr_label(metric), expr_label(additional_attrs))

  # created object must be recognized as a valid `tbl_mbte`
  expect_true(is_tbl_mbte(obj), info = info)
  expect_true(is_tibble(obj), info = info)

  # relevant attributes must match expectated values
  expect_identical(attr_time(obj), time, info = info)
  expect_identical(attr_value(obj), value, info = info)
  expect_identical(attr_signal(obj), signal, info = info)
  expect_identical(attr_fits(obj), fits, info = info)
  expect_identical(attr_metric(obj), metric, info = info)

  # make sure generated object inherits from all the specified subclasses
  purrr::walk(subclass, ~expect_is(obj, .x, info = info))

  # check if additional attributes have been set correctly
  if (length(additional_attrs) != 0) {
    stopifnot(is_named(additional_attrs)) # no unnamed elements allowed
    purrr::iwalk(additional_attrs, ~{
      expect_identical(attr(obj, .y), .x, info = info)
    })
  }
}

# test for a table created with defaults arguments (signal = "signal", etc.)
test_default_table <- function(obj, ds_name, time, value, ...) {
  time <- ensym(time)
  value <- ensym(value)
  test_new_tbl_mbte(obj, ds_name, !!time, !!value, signal = "signal",
    fits = "fits", metric = "metric", ...)
}

# only randomize `time` and `value`-attributes (in order to test defaults)
test_tv_random <- function(dataset, ds_name) {
  time <- gen_random_string()
  value <- gen_random_string()
  subclass <- gen_random_string(8)
  additional_attrs <- list(custom_attribute = sample(1L:100L, 1))

  # internal testing function
  test_tv_random_ <- function(time, value) {
    obj <- new_tbl_mbte(dataset, !!time, !!value, subclass = subclass,
      custom_attribute = additional_attrs$custom_attribute)
    test_default_table(obj, ds_name, !!time, !!value,
      additional_attrs = additional_attrs, subclass = subclass)
  }

  # pass `time` and `value` as strings
  test_tv_random_(time, value)
  # pass `time` and `value` as symbols
  test_tv_random_(rlang::sym(time), rlang::sym(value))
}

# test dataset creation with all symbol-related attributes generated
# pseudo-randomly
test_all_random <- function(dataset, ds_name) {
  time <- gen_random_string()
  value <- gen_random_string()
  signal <- gen_random_string()
  fits <- gen_random_string()
  metric <- gen_random_string()
  subclass <- gen_random_string(8)
  additional_attrs <- list(custom_attribute = sample(1L:100L, 1))

  # internal testing function
  test_all_random_ <- function(time, value, signal, fits, metric) {
    obj <- new_tbl_mbte(dataset, !!time, !!value, signal = !!signal,
      fits = !!fits, metric = !!metric, subclass = subclass,
      custom_attribute = additional_attrs$custom_attribute)
    test_new_tbl_mbte(obj, ds_name, !!time, !!value, !!signal, !!fits, !!metric,
      additional_attrs = additional_attrs, subclass = subclass)
  }

  # pass as strings
  test_all_random_(time, value, signal, fits, metric)
  # pass as symbols
  test_all_random_(rlang::sym(time), rlang::sym(value), rlang::sym(signal),
    rlang::sym(fits), rlang::sym(metric))
}

# create a randomized dataset (ensure reproducibility via testing seed)
withr::with_seed(testing_seed(), {
  raw_dataset <- gen_raw_dataset()
})

# # NOTE: data.frame's are provided as inputs to assure, that the conversion to
# a tibble gets properly done

test_that("time value randomized - df", {
  datasets <- list(
    iris_df = iris,
    default_raw_df = as.data.frame(raw_dataset)
  )

  # run tests (randomise time and value)
  withr::with_seed(testing_seed(), {
    purrr::iwalk(datasets, test_tv_random)
  })
})

test_that("time value randomized - tibble", {
  datasets <- list(
    iris_tbl = as_tibble(iris),
    default_raw_tbl <- raw_dataset
  )

  # run tests (randomise time and value)
  withr::with_seed(testing_seed(), {
    purrr::iwalk(datasets, test_tv_random)
  })
})

test_that("all randomized - df", {
  datasets <- list(
    iris_df = iris,
    default_raw_df = as.data.frame(raw_dataset)
  )

  # randomise all symbol-related variables (time, value, signal, fits, metric)
  withr::with_seed(testing_seed(), {
    purrr::iwalk(datasets, test_all_random)
  })
})

test_that("all randomized - tibble", {
  datasets <- list(
    iris_tbl = as_tibble(iris),
    default_raw_tbl <- raw_dataset
  )

  # randomise all symbol-related variables (time, value, signal, fits, metric)
  withr::with_seed(testing_seed(), {
    purrr::iwalk(datasets, test_all_random)
  })
})

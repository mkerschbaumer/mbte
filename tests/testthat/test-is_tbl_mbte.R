context("is_tbl_mbte")

# generate a randomized tbl_mbte, that should be recognized as valid.
gen_valid_tbl_mbte <- function(dat = iris) {
  dat <- tibble::as_tibble(dat)

  # set relevant attributes to random symbols
  attr(dat, "time") <- gen_random_sym()
  attr(dat, "value") <- gen_random_sym()
  attr(dat, "signal") <- gen_random_sym()
  attr(dat, "fits") <- gen_random_sym()

  class(dat) <- c("tbl_mbte", class(dat))

  dat
}

test_that("is_tbl_mbte - positive test helper", {
  # NOTE: attribute-values are set to random strings (ensure no assumptions
  # about column names are made)
  withr::with_seed(testing_seed(), {
    obj <- gen_valid_tbl_mbte()
  })
  expect_true(is_tbl_mbte(obj))
})

# cross-check, that objects created by new_tbl_mbte() get detected as valid
# `tbl_mbte`-objects
test_that("is_tbl_mbte - via new_tbl_mbte()", {
  withr::with_seed(testing_seed(), {
    obj <- gen_raw_dataset()
  })
  expect_true(is_tbl_mbte(new_tbl_mbte(obj, time, value)))
})

test_that("is_tbl_mbte - not a tibble", {
  withr::with_seed(testing_seed(), {
    obj <- as.data.frame(gen_valid_tbl_mbte())
  })
  class(obj) <- c("tbl_mbte", class(obj))

  # make sure symbols are still present
  expect_true(rlang::is_symbol(attr_time(obj)))
  expect_true(rlang::is_symbol(attr_value(obj)))
  expect_true(rlang::is_symbol(attr_signal(obj)))
  expect_true(rlang::is_symbol(attr_fits(obj)))

  expect_false(is_tibble(obj))
  expect_false(is_tbl_mbte(obj)) # tibble expected by is_tbl_mbte()
})

test_that("is_tbl_mbte - wrong class", {
  withr::with_seed(testing_seed(), {
    obj <- gen_valid_tbl_mbte()
  })
  class(obj) <- setdiff(class(obj), "tbl_mbte")
  expect_false(is_tbl_mbte(obj)) # should be of class `tbl_mbte` but is not
})

# helper function to malformat an attribute (which is expected to be a symbol),
# assert that the resulting object is not seen as a `tbl_mbte`.
test_attribute_malformatted <- function(which) {
  stopifnot(purrr::is_scalar_character(which))
  withr::with_seed(testing_seed(), {
    obj <- gen_valid_tbl_mbte()
  })

  # NOTE: reproducibility of the gen_random_string()-call is not important
  # here (since is_tbl_mbte() must only detect, that the target attribute
  # is not a symbol)
  to_test <- list(gen_random_string(), 123, NA, NaN, Inf, NULL)
  purrr::walk(to_test, ~{
    attr(obj, which) <- .x
    expect_false(is_tbl_mbte(obj),
      info = paste("input:", which, expr_label(.x))
    )
  })
}

test_that("is_tbl_mbte - attributes malformatted", {
  # all tested attributes below must be symbols
  attr_to_test <- c("time", "value", "signal", "fits")

  withr::with_seed(testing_seed(), {
    purrr::walk(attr_to_test, test_attribute_malformatted)
  })
})


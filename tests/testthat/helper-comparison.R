# use are_deep_equal() to verify, that two objects match
#
# NOTE: `...` is passed to are_deep_equal() - will be passed to paste0();
# is used to generate additional information about what has been passed to
# expect_deep_equal()
expect_deep_equal <- function(obj, exp, ...) {
  expect_true(are_deep_equal(obj, exp, ...))
}

# check if an object (`obj`) is equal to an expected object by recursively
# applying are_deep_equal() to the corresponding subelements; NOTES:
#
# + recursion is applied to list-like objects (when `is.list(obj)` evaluates
#   to `TRUE`)
# + for objects to be considered equal, the following condition must apply:
#   + the classes, names and length must be equal
#   + elements not being lists must be considered equal by the specified
#     comparison function (`comp_fun`)
#
# `...` is passed to paste0() in order to generate additional information in
# case of failure.
#
# `comp_fun` is the actual comparison function used to compare two non-list-like
# objects.
#
# NOTE: this function is intended to test equality of two tibbles containing
# list-columns
are_deep_equal <- function(obj, exp, ..., nesting = NULL,
                           comp_fun = all.equal) {
  # generate information about nesting level
  info <- paste0(
    "nesting:", ifelse(length(nesting) == 0, " none", nesting),
    " - info from caller: \"", ..., "\""
  )

  # objects can't be equal if classes don't match
  if (!identical(class(obj), class(exp))) {
    testthat::fail(paste("Class mismatch at", info))
    return(FALSE)
  }

  # NOTE: exp doesen't need to be checked (since objects are of the same
  # classes)
  if (!is.list(obj)) {
    # perform "regular" comparison (of non-list elements)
    if (!isTRUE(comp_fun(obj, exp))) {
      testthat::fail(paste("Comparision for non-list objects failed at", info))
      return(FALSE)
    } else {
      # objects are equal
      return(TRUE)
    }
  }

  if (!identical(length(obj), length(exp))) {
    testthat::fail(paste("Length mismatch at", info))
    return(FALSE)
  }

  if (!identical(names(obj), names(exp))) {
    testthat::fail(paste("Names mismatch at", info))
    return(FALSE)
  }

  names <- names(obj)
  # if no names are present (e.g. in an unnamed list), the positions are used
  # as names (converted to character)
  to_compare <- list(
    name = if (length(names) != 0) names else as.character(seq_along(obj)),
    index = seq_along(obj)
  )

  # compare subelements via index
  purrr::pmap(to_compare, function(name, index) {
    are_deep_equal(obj[[index]], exp[[index]], ...,
      comp_fun = comp_fun,
      nesting = c(nesting, name)
    )
  })

  # tests haven't failed ==> objects are equal
  TRUE
}

# Test if 2 `tbl_mbte`-objects are equal - needed if pairwise comparison of
# list-columns is required - result of a computation (`res`) is checked against
# expected result (`exp`).
expect_tbl_mbte_equal <- function(res, exp, ...) {
  expect_true(is_tbl_mbte(res))
  expect_true(is_tbl_mbte(exp))

  # check relevant attributes
  expect_identical(attr_time(res), attr_time(exp))
  expect_identical(attr_value(res), attr_value(exp))
  expect_identical(attr_signal(res), attr_signal(exp))
  expect_identical(attr_fits(res), attr_fits(exp))

  # column-names and dimensions must match (asserted before performing deep
  # comparison involving the contents of list-columns)
  expect_equal(dim(res), dim(exp))
  expect_equal(colnames(res), colnames(exp))

  # make sure no column-mismatches are present (e.g. column A of table 1 not
  # identical to column A of table 2)
  expect_deep_equal(res, exp, ...)
}

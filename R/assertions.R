# assert that certain prerequisites are given and throw a corresponding error
# otherwise

# make sure `x` is named. x_sym refers to the "name" of x in the context of the
# calling function
#' @importFrom rlang is_named
assert_all_named <- function(x, ..., x_sym = substitute(x)) {
  if (!is_named(x)) {
    stop(err_expected_named(x_sym, ...))
  }
}

# specialisation fo assert_all_named() - fills in "..." for x_sym
#' @importFrom rlang sym
assert_ellipsis_named <- function(x, ...) {
  assert_all_named(x, ..., x_sym = rlang::sym("..."))
}

# make sure an object is grouped (using dplyr::is_grouped_df())
#' @importFrom dplyr is_grouped_df
assert_grouped <- function(x, ..., x_sym = substitute(x)) {
  if (!is_grouped_df(x)) {
    stop(err_expected_grouped(x_sym, ...))
  }

  NULL
}

# make sure `colname` (anything that can be converted to a character) is
# present in dataset
assert_has_column <- function(x, colname, ..., x_sym = substitute(x)) {
  colname <- as.character(colname)
  if (!colname %in% colnames(x)) {
    stop(err_col_expected(x_sym, colname, ...))
  }
}

# TODO: implement for unified error messages
#' @importFrom purrr partial
assert_has_time_column <- partial(assert_has_column, .msg = "(time-column)")
assert_has_value_column <- partial(assert_has_column, .msg = "(value-column)")
assert_has_signal_column <- partial(assert_has_column, .msg = "(signal-column)")
assert_has_fits_column <- partial(assert_has_column, .msg = "(fits-column)")

# check if x inherits from an expected class and raise an error if it doesn't
assert_class <- function(x, exp_class, ..., x_sym = substitute(x)) {
  if (!inherits(x, exp_class)) {
    stop(err_class_mismatch(x_sym, exp_class = exp_class, ...))
  }
}

# similar to assert_class(): provide a condition-function (if it returns
# `FALSE`, it is assumed, that the input `x` is faulty)
assert_class_callback <- function(condition, description) {
  function(x, ..., x_sym = substitute(x)) {
    if (!condition(x)) {
      stop(err_class_mismatch(x_sym, description, ...))
    }
  }
}

# specialised assertions regarding classes
assert_is_character <- assert_class_callback(is.character, "is not a character")
assert_is_numeric <- assert_class_callback(is.numeric, "is not numeric")
assert_is_integer <- assert_class_callback(is.integer, "is not an integer-vector")
assert_is_function <- assert_class_callback(is.function, "is not a function")
assert_is_list <- assert_class_callback(is.list, "is not a list")
# is scalar numeric (numeric vector of length 1)
assert_is_scalar_num <- assert_class_callback(function(x) {
  is.numeric(x) && length(x) == 1
}, "is not a scalar numeric")

assert_is_tbl_mbte <- function(x, ..., x_sym = substitute(x)) {
  if (!is_tbl_mbte(x)) {
    stop(err_not_tbl_mbte(x_sym, ...))
  }
}

# a function for creating assertion helpers to check the integrity of specific
# columns via other assertions. `...` in this context takes assertions
# (e.g. assert_is_numeric); description should be a string, which describes
# the column being checked (e.g. "time") - gets passed to assertion
#
# the generated assertion helpers convert `colname` to a character.
# NOTE: the `x_sym`-argument, which is passed on to the assesrtions provided
# in `...` gets modified: e.g. colname = "custom_column", x_sym = "x"
# ==> new x_sym = x$custom_column
#' @importFrom purrr walk
#' @importFrom rlang expr
assert_valid_column <- function(description, ...) {
  assertions <- list(...)
  function(x, colname, x_sym = substitute(x)) {
    walk(assertions, ~{
      .x(x[[as.character(colname)]], description,
         x_sym = expr(`$`(!!x_sym, !!colname)))
    })
  }
}

# abstract out validity checking of columns
assert_valid_time_col <- assert_valid_column("(time-column)", assert_is_numeric)
assert_valid_value_col <- assert_valid_column("(value-column)", assert_is_numeric)
assert_valid_signal_col <- assert_valid_column("(signal-column)", assert_is_list)
assert_valid_fits_col <- assert_valid_column("(fits-column)", assert_is_list)

assert_equal_lengths <- function(x1, x2, ..., x1_sym = substitute(x1),
                                 x2_sym = substitute(x2)) {
  len_1 <- length(x1)
  len_2 <- length(x2)
  if (len_1 != len_2) {
    # generate descriptive string of how the lengths differ and include it in
    # the error message
    len_desc = sprintf("length(%s) = %d, length(%s) = %d", as.character(x1_sym),
      len_1, as.character(x2_sym), len_2)
    stop(err_dim_incomp(x1_sym, x2_sym, len_desc, ...))
  }
}
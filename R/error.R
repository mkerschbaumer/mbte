# error-handling related functions
#
# in this family of functions `x_sym` denotes a symbol/expression of an object,
# which is faulty.
# ... gets passed on to `mbte_error` (allows to append text to the error
# message)

# base error class
base_err_class <- "mbte_error"

# construct an error inheriting from `simpleError`, base_err_class and
# optional subclass
#' @importFrom purrr invoke
mbte_error <- function(..., subclass = NULL) {
  err <- simpleError(invoke(paste, list(...)))
  class(err) <- c(subclass, base_err_class, class(err))
  err
}

#' @importFrom rlang expr_label
err_expected_named <- function(x_sym, ...) {
  mbte_error("All elements of", expr_label(x_sym), "must be named", ...,
    subclass = "err_expected_named")
}

# target object is not grouped (by dplyr::group_by())
#' @importFrom rlang expr_label
err_expected_grouped <- function(x_sym, ...) {
  mbte_error(expr_label(x_sym), "must be grouped", ...,
    subclass = "err_expected_grouped")
}

# a column is expected in a dataset, but not present
#' @importFrom rlang expr_label
err_col_expected <- function(x_sym, colname, ...) {
  mbte_error(expr_label(x_sym), "must contain column", paste0('"', colname, '"'),
    ..., subclass = "err_col_expected")
}

# object `x_sym` is not of class exp_class
#' @importFrom rlang expr_label
err_class_mismatch <- function(x_sym, ..., exp_class) {
  if (!missing(exp_class)) {
    mbte_error(expr_label(x_sym), "does not inherit from class", exp_class, ...,
      subclass = "err_class_mismatch")
  } else {
    mbte_error(expr_label(x_sym), ..., subclass = "err_class_mismatch")
  }
}

# special case of err_class_mismatch(): object is not a `tbl_mbte`
#' @importFrom rlang expr_label
err_not_tbl_mbte <- function(x_sym, ...) {
  mbte_error(expr_label(x_sym), "must be a tbl_mbte", ...,
    subclass = c("mbte_not_tbl_mbte", "mbte_class_mismatch"))
}

# error while evaluating a fitting quosure or while predicting the signal-values
err_fit <- function(...) {
  mbte_error("Fitting failed", ..., subclass = "err_fit")
}

# wrap errors raised by evaluating the captured quosure via custom wrapper
# function
#' @importFrom rlang enquo eval_tidy
eval_error_wrapper <- function(expr, .wrapper) {
  expr <- enquo(expr)
  tryCatch(eval_tidy(expr), error = function(e) {
    wrapped_err <- .wrapper("- original error message:", paste0('"', e$message, '"'))
    stop(wrapped_err)
  })
}

# dimensions incompatible between objects x1 and x2
#' @importFrom rlang expr_label
err_dim_incomp <- function(x1_sym, x2_sym, ...) {
  mbte_error("Incompatible dimensions of objects", expr_label(x1_sym),
    "and", expr_label(x2_sym), ..., subclass = "err_dim_incomp")
}
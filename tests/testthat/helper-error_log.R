# expect unusual events happening during execution of `expr` (gets quoted)
#
# `gen_check`, if provided, gets passed to purrr::as_mapper() and is intended to
# be used for checking the *general* integrity of the event log (e.g. checking
# consistency with known values). Its first argument should take the extracted
# event log.
#
# `err_check`, if provided, is meant to check errors from the event log
# (event_log$error). Like `gen_check`, it also gets passed to purrr::as_mapper()
# first. This function may take 2 arguments (first argument: recorded error;
# second argument: row of the event-log tibble, where the error can be found).
#
# This function returns the result from the computation of `expr` invisibly.
with_event_log <- function(expr, gen_check, err_check) {
  expr <- rlang::enquo(expr)

  # expect "unusual" events, which are recorded in the event log (warning should
  # inform user)
  res <- expect_warning(!!expr, regexp = "mbte_event_log\\(\\)")

  # make sure event log is valid
  event_log <- attr_event_log(res)
  expect_true(tibble::is_tibble(event_log))
  expect_true(nrow(event_log) != 0)

  # run generic consistency checks of event log (if provided)
  if (!missing(gen_check)) {
    gen_check <- purrr::as_mapper(gen_check)
    gen_check(event_log)
  }

  # check every recorded event via a checking function (if provided)
  if (!missing(err_check)) {
    err_check <- purrr::as_mapper(err_check)
    expect_true(is.list(event_log$error))
    purrr::iwalk(event_log$error, err_check)
  }

  # return result of computation
  invisible(res)
}


#### error checking helpers #####

# helper functions to construct error-checking functions (meant to be used
# in conjunction with `with_event_log` (as "err_check"-argument))

# create a checking function, that tests if an error inherits from the specified
# class and its message matches the provided error-message (regexp).
create_err_checker <- function(errmsg, class) {
  function(.x, .y) {
    info <- paste("errorlog mismatch: error nr", .y)
    expect_is(.x, class, info = info)
    expect_match(.x$message, errmsg, info = info)
  }
}

# a specialized checking function for error class "err_class_mismatch"
err_class_mismatch_checker <- function(errmsg) {
  create_err_checker(errmsg, class = "err_class_mismatch")
}

# specialized for checking if an error inherits from "err_fit"
#
# NOTE: the provided error-message is passed with modifications (because the
# prefix "original.+message.+" should be present)
err_fit_checker <- function(errmsg) {
  create_err_checker(paste0("original.+message.+", errmsg), class = "err_fit")
}

# a checker for "err_dim_incomp"-errors
err_dim_incomp_checker <- function(errmsg) {
  create_err_checker(errmsg, class = "err_dim_incomp")
}

# an error checker for "err_eval_metric"
#
# NOTE: the provided error message gets appended to a prefix
err_eval_metric_checker <- function(errmsg) {
  create_err_checker(paste0("original.+error.+", errmsg), "err_eval_metric")
}
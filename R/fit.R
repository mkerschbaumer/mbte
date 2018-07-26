#' Fit models to signals
#'
#' @param x A `tbl_mbte`, where signals are present as subtibbles in a
#' list-column.
#' @param ... It is assumed, that the ellipsis only contains named elements. The
#' elements are used as quosures and tidy evaluation is used. Caution:
#' `.signal`, `.time_sym` and `.value_sym` are masked (See  details for more
#' information.).
#' Closures may return a numeric vector with the length of the original
#' signal-values or an object with a method for \code{\link[stats]{predict}}.
#' If the quosure evaluates to a numeric vector, it gets used as is. Otherwise,
#' \code{predict(returned_value, newdata = .signal)} will be called, in order to
#' get the predictions.
#'
#' @details
#' Quosures can make use of masked objects, that shadow their normal regular
#' execution environment:
#' \describe{
#'   \item{.signal}{A \code{\link[tibble]{tibble}} with the signal to fit
#'   (time and value are present)}
#'   \item{.time_sym}{The name of the time column as a
#'   \code{\link[base]{name}}.}
#'   \item{.value_sym}{The name of the value-column stored as a
#'   \code{\link[base]{name}}}
#' }
#'
#' @include extract_subsignals.R
#' @inheritSection mbte_extract_subsignals event-logging
#'
#' @section event-table:
#' The error-log table contains the following columns:
#' \describe{
#'   \item{error}{The error, that occurred during processing. Errors which
#'     originate from the evaluation of a fitting quosure (specified in
#'     \code{...}) or prediction-related errors are wrapped.}
#'   \item{row_nr}{The row-number of the original table (\code{x}), at which
#'     the error occurred.}
#'   \item{signal}{The signal-subtibble processed at the time the error
#'     occurred.}
#'   \item{fit_name}{The name of the fitting quosure (name in \code{...}).}
#'   \item{fit_quo}{The fitting quosure being processed (element of \code{...}).}
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom purrr imap imap_dfc
#' @importFrom rlang enquos eval_tidy new_environment sym
#' @importFrom stats predict
#' @export
mbte_fit <- function(x, ...) {
  assert_is_tbl_mbte(x)
  fit_quos <- enquos(...)
  assert_ellipsis_named(fit_quos)

  # extract symbol-related attributes
  time <- attr_time(x)
  value <- attr_value(x)
  signal <- attr_signal(x)
  fits <- attr_fits(x)

  # assert presence and type of signal column
  assert_has_column(x, signal, "(signal-column)")
  assert_valid_signal_col(x, signal)

  # create data mask for tidy evaluation
  mask <- new_environment(list(.time_sym = time, .value_sym = value))

  # create dummy symbol (for displaying-purposes for a error message)
  sig_sym <- sym("sig")

  # to log the first 50 errors
  event_store <- new_event_store(50L)

  # fit a fitting-quosure to a signal
  fit_quo_to_signal <- function(fit_quo, fit_name, sig) {
    # assert signal contains valid time-and value-columns
    assert_has_column(sig, time)
    assert_has_column(sig, value)
    assert_valid_time_col(sig, time)
    assert_valid_value_col(sig, value)

    # add signal tibble to masking-environment
    mask$.signal <- sig

    # wrap errors during quosure-evaluation or during signal-values prediction
    # in `err_fit`-class`
    fit <- eval_error_wrapper(.wrapper = err_fit, {
      fit <- eval_tidy(fit_quo, data = mask)

      # get predictions via predict(), if the quosure didn't evaluate to a numeric
      # vector
      if (!is.numeric(fit)) {
        fit <- predict(fit, newdata = sig)
      }
      # return fitted values
      fit
    })

    # make sure the predicted signal-values are numeric and match with the
    # original signal-values (in terms of length)
    assert_is_numeric(fit)
    assert_equal_lengths(fit, sig[[value]], x2_sym = sig_sym)

    # return fitted values
    fit
  }

  # returns a numeric vector of the length of the signal-values in any case
  # (errors are stored in `event_store`)
  safe_fit <- function(fit_quo, fit_name, sig, ...) {
    tryCatch(fit_quo_to_signal(fit_quo, fit_name, sig), error = function(e) {
      event_store$add_event(error = e, ..., signal = sig, fit_name = fit_name,
        fit_quo = fit_quo)

      # return NA's for predicted signal-values (since error occurred)
      rep(NA_real_, nrow(sig))
    })
  }

  # fit specific signal via specified fitting quosures
  fit_signal_impl <- function(sig, row_nr) {
    # fit fitting-quosures to signal
    imap_dfc(fit_quos, safe_fit, sig = sig, row_nr = row_nr)
  }

  # safely fit signals
  .results <- imap(x[[signal]], fit_signal_impl)

  x %>%
    mutate(!!fits := .results) %>%
    mbte_reconstruct(x) %>% # to preserve `tbl_mbte`-class
    cond_add_event_store(event_store, mbte_fit) # add error-log conditionally
}

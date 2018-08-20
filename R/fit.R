#' Fit models to signals
#'
#' The presence of the `signal`-column is required (see \code{\link{tbl_mbte}}).
#' The idea is to fit the values of a signal via arbitrary models. Those
#' models are used to make predictions for the signal-values.
#'
#' @param x A \code{\link{tbl_mbte}}.
#' @param ... It is assumed, that the ellipsis only contains named elements. The
#'   elements are used as \code{\link[rlang:quotation]{quosures}} via
#'   \code{\link[rlang:eval_tidy]{tidy evaluation}}. Caution: `.signal`,
#'   `.time_sym` and `.value_sym` are masked (See details for more information).
#'   The ellipsis supports \code{\link[rlang]{tidy-dots}}-semantics.
#'
#' @details
#' Fitting-quosures can make use of the following masked objects (see examples
#' for clarification):
#' \describe{
#'   \item{.signal}{A \code{\link[tibble]{tibble}} with the signal to fit
#'     (`time` and `value`-columns are present).}
#'   \item{.time_sym}{The name of the time column as a
#'     \code{\link[base:name]{symbol}}.}
#'   \item{.value_sym}{The name of the value-column stored as a
#'     \code{\link[base:name]{symbol}}.}
#' }
#'
#' Currently, there are two ways, how a fitting-quosure may return the
#' predicted signal-values for a signal:
#' \itemize{
#'   \item by returning a numeric vector: if a quosure evaluates to a numeric
#'     vector, the values are used as is.
#'   \item by returning an object compatible with \code{\link[stats]{predict}}:
#'     In this case, \code{predict()} will be called on the returned object
#'     with \code{newdata = .signal} (see above; the original signal-table,
#'     which has been used for fitting, gets passed). This option may come in
#'     handy, if the actual function used for fitting (e.g.
#'     \code{\link[stats]{lm}}) returns an object, for which a predict-method
#'     returning a numeric vector exists (in this case
#'     \code{\link[stats]{predict.lm}}).
#' }
#'
#' In either case, the length of the vector containing the values of the
#' predicted signal has to match the number of rows of \code{.signal} (the
#' signal-table used for fitting).
#'
#' @return
#' The original table gets returned with the `fits`-column added (list column
#' consisting of tibbles). The column names are the names of the ellipsis
#' (\code{...}). Each fit is a numeric column containing the predicted
#' signal-values.
#'
#' @include extract_subsignals.R
#' @inheritSection mbte_extract_subsignals event-logging
#'
#' @section event-log:
#' The tibble containing event-information contains the following columns:
#' \describe{
#'   \item{error}{The error, that occurred during processing. Errors which
#'     originate from the evaluation of a fitting quosure or prediction-related
#'     errors get wrapped.}
#'   \item{row_nr}{The row-number of the original table \code{x}, at which
#'     the error occurred.}
#'   \item{signal}{The signal-subtibble processed at the time the error
#'     occurred.}
#'   \item{fit_name}{The name of the fitting quosure (name in \code{...}).}
#'   \item{fit_quo}{The fitting quosure being processed (element of \code{...}).}
#' }
#'
#' @seealso \code{\link{filtered_signals}} (dataset used in examples)
#' @examples
#' # load dataset (tbl_mbte with extracted subsignals)
#' data(filtered_signals)
#'
#' # fit linear models to signals (by returning a predict()-compatible object
#' # and by returning a numeric vector of the correct length)
#' #
#' # NOTE: `.signal` is not defined in this scope. However, masking is used and
#' # to provide the signal-tibble as described above.
#' fits1 <- mbte_fit(filtered_signals,
#'   lm1 = lm(value ~ t, .signal), # rely on predict()
#'   lm2 = predict(lm(value ~ t, .signal)) # return numeric vector
#' )
#'
#' # resuting table with `fits`-column added
#' fits1
#'
#' # a tibble in the `fits`-list column; the columns `fit1` and `fit2` are
#' # equivalent (only different ways of returning the fittind signal-values have
#' # been used).
#' fits1$fits[[1]]
#'
#' # use tidy-dots semantics of mbte_fit() - useful if fitting quosures are
#' # generated programmatically
#'
#' # define fitting quosures
#' fitting_candidates <- rlang::quos(
#'   lm1 = lm(value ~ t, .signal), # rely on predict()
#'   lm2 = predict(lm(value ~ t, .signal)) # return numeric vector
#' )
#'
#' # use tidy-dots splicing (equivalent to call producing `fits1` above)
#' fits2 <- mbte_fit(filtered_signals, !!!fitting_candidates)
#' fits2
#'
#' fits2$fits[[1]]
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
  assert_has_signal_column(x, signal)
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
    assert_has_time_column(sig, time)
    assert_has_value_column(sig, value)
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

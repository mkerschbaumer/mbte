#' Compute metrics for a \code{\link{tbl_mbte}} with fitted signals
#'
#' The presence of the `signal`- and `fits`-columns is required. An (error)-
#' metric is computed based on the fitted signal-values and the original signal-
#' values. This gets done in order to allow a comparison of different
#' fitting-methods or to detect trends (e.g. an error metric gets used. A low
#' error indicates, that the a model has been able to generalize the underlying
#' trend).
#'
#' @param x A \code{\link{tbl_mbte}}
#' @param ... The ellipsis must only contain named elements. The elements are
#'   used as \code{\link[rlang:quotation]{quosures}} via
#'   \code{\link[rlang:eval_tidy]{tidy evaluation}}. The quosures should perform
#'   the metric computation. Caution: `.pred` and `.obs` are masked (See details
#'   for more information). \code{\link[rlang]{tidy-dots}}-semantics are
#'   supported.
#'
#' @details
#' The metric quosures can use the following masked objects (see examples):
#' \describe{
#'   \item{.pred}{The predicted signal-values}
#'   \item{.obs}{The observed/measured (original) signal-values}
#' }
#'
#' Both masked objects (\code{.pred} and \code{.obs}) are numeric vectors.
#'
#' A metric quosure must evalutate to a scalar numeric (double or integer-vector
#' of length 1). Otherwise or if an error is encountered, \code{NA_real_} will
#' be the result of the metric computation.
#'
#' @return
#' A tibble with the following columns:
#' \describe{
#'   \item{desc}{In this case, `desc` is a placeholder for all the columns
#'     in \code{x}, which are not columns for `signal` or `fits` (can be seen as
#'     descriptive columns).}
#'   \item{fit}{A character column with the name of the fits. Its elements will
#'     be the names of the fitting quosures used (in most cases the names of
#'     the fitting quosures in the call to \code{\link{mbte_fit}}).}
#'   \item{metric}{The name of the metric-quosure, which computed the result.}
#'   \item{result}{The actual result from the metric-computation (numeric or
#'     integer, depending on what type the metric quosures returned).}
#' }
#'
#' @include extract_subsignals.R
#' @inheritSection mbte_extract_subsignals event-logging
#'
#' @section event-log:
#' A tibble containing event-information with the following columns:
#' \describe{
#'   \item{error}{The error, which occurred during processing. Errors occurring
#'     during the evaluation of a metric-quosure are wrapped.}
#'   \item{row_nr}{The row-number of the original table (\code{x}), at which the
#'     error occurred.}
#'   \item{fit_name}{The name of the fit.}
#'   \item{metric_name}{The name of the current metric (a name in \code{...}).
#'     NOTE: "current" referes to the moment the error/event occurred).}
#'   \item{metric_quo}{The current metric-quosure (element of \code{...}).}
#'   \item{pred}{The current predicted signal-values (expected to be be
#'     numeric).}
#'   \item{obs}{The observed signal-values (of the original signal).}
#' }
#'
#' NOTE: currently only errors are logged.
#'
#' @seealso \code{\link{filtered_signals}} (dataset used in examples)
#' @examples
#' data(filtered_signals)
#' filtered_signals
#'
#' # fit linear model to each signal (`t` denotes the time column)
#' fits <- mbte_fit(filtered_signals, lm = lm(value ~ t, .signal))
#'
#' # define error metric (in this case normalized root mean squared error)
#' nrmse <- function(pred, obs) {
#'   sqrt(mean((pred - obs)^2)) / (max(obs) - min(obs))
#' }
#'
#' # compute metrics
#' # NOTE: `.pred` and `.obs` not present in scope, but provided via masking
#' metrics <- mbte_compute_metrics(fits, nrmse = nrmse(.pred, .obs))
#' metrics
#'
#' @importFrom dplyr mutate select ungroup
#' @importFrom magrittr "%>%"
#' @importFrom purrr imap_dfr pmap
#' @importFrom rlang enquos eval_tidy new_environment
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export
mbte_compute_metrics <- function(x, ...) {
  assert_is_tbl_mbte(x)
  metric_quos <- enquos(...)

  # all elements of ellipsis must be named
  assert_ellipsis_named(metric_quos)

  # extract symbols from object
  value <- attr_value(x)
  signal <- attr_signal(x)
  fits <- attr_fits(x)

  # check integrity of columns
  assert_has_signal_column(x, signal)
  assert_valid_signal_col(x, signal)

  # create masking environment for `.pred` and `.obs`
  mask <- new_environment()

  # event store for errors (only collect first 50 errors)
  event_store <- new_event_store(50L)

  compute_metric <- function(metric_quo, metric_name, fit_name, ...) {
    # make sure a scalar numeric gets returned in every case
    result <- tryCatch({
      # evaluate metric quosure; if an error occurrs wrap it in an
      # `err_eval_metric`-error
      res <- eval_error_wrapper(
        eval_tidy(metric_quo, data = mask),
        .wrapper = err_eval_metric
      )
      assert_is_scalar_num(res, "- result returned from metric quosure")
      res
    }, error = function(e) {
      # store occurred error with additional context
      event_store$add_event(
        error = e, ..., fit_name = fit_name,
        metric_name = metric_name, metric_quo = metric_quo,
        pred = mask$.pred, obs = mask$.obs
      )

      NA_real_
    })

    tibble(fit = fit_name, metric = metric_name, result = result)
  }

  process_fit <- function(fit_value, fit_name, ...) {
    mask$.pred <- fit_value # predicted value of signal
    imap_dfr(metric_quos, compute_metric, fit_name = fit_name, ...)
  }

  create_metric_subtibble <- function(sig, all_fits, row_nr) {
    mask$.obs <- sig[[value]] # value of original signal
    # compute metrics for all fits for a specific signal
    imap_dfr(all_fits, process_fit, row_nr = row_nr)
  }

  # list to loop over signal-fit pairs with row indices
  signals_fits <- list(x[[signal]], x[[fits]], seq_len(nrow(x)))

  x %>%
    # explicitly remove grouping, since metric is computed via the signal
    # and the fit of a row (groups irrelevant)
    ungroup() %>%
    mutate(.metric_subtibbles = pmap(signals_fits, create_metric_subtibble)) %>%
    select(-!!signal, -!!fits) %>%
    unnest(.metric_subtibbles) %>%
    cond_add_event_store(event_store, mbte_compute_metrics)
}

# ignore NOTE from R CMD check (since `.metric_subtibbles` gets created by
# the mutate()-call above)
globalVariables(".metric_subtibbles")

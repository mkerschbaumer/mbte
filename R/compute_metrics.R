#' Compute metrics based on the fitted signals and the original signal-values.
#'
#' @param x A `tbl_mbte`, where predicted signals and original signals are
#' present.
#' @param ... The ellipsis must only contain named elements. The elements are
#' used as quosures and tidy evaluation is used. Caution:
#' `.pred` and `.obs` are masked (See details for more information.).
#' Passed closures must evaluate to a scalar numeric (type double or integer).
#' Otherwise or if an error is encountered, \code{NA_real_} will be returned.
#'
#' @details
#' The following objects are masked via a data-mask:
#' \describe{
#'   \item{.pred}{The predicted signal-values}
#'   \item{.obs}{The observed (original) signal-values}
#' }
#'
#' @include extract_subsignals.R
#' @inheritSection mbte_extract_subsignals event-logging
#' @section event-table:
#' The error-log (tibble) contains the following columns:
#' \describe{
#'   \item{error}{The error, which occurred during processing. Errors occurring
#'     during the evaluation of a metric-quosure (a element passed to
#'     \code{...}) are wrapped.}
#'   \item{row_nr}{The row-number of the original table (\code{x}), at which the
#'     error occurred.}
#'   \item{fit_name}{The name of the fit.}
#'   \item{metric_name}{The name of the current metric (a name in \code{...}).}
#'   \item{metric_quo}{The current metric-quosure (element of \code{...}).}
#'   \item{pred}{The current predicted signal-values (expected to be be
#'     numeric).}
#'   \item{obs}{The observed signal-values (of the original signal).}
#' }
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
  assert_has_column(x, signal, "(signal-column)")
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
      res <- eval_error_wrapper(eval_tidy(metric_quo, data = mask),
        .wrapper = err_eval_metric)
      assert_is_scalar_num(res, "- result returned from metric quosure")
      res
    }, error = function(e) {
      # store occurred error with additional context
      event_store$add_event(error = e, ..., fit_name = fit_name,
        metric_name = metric_name, metric_quo = metric_quo,
        pred = mask$.pred, obs = mask$.obs)

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
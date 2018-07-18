#' Compute metrics based on the fitted signals and the original signal-values.
#'
#' @param x A `tbl_mbte`, where predicted signals and original signals are
#' present.
#' @param ... The ellipsis must only contain named elements. The elements are
#' used as quosures and tidy evaluation is used. Caution:
#' \code{.pred} and \link{.obs} are masked (See
#' details for more information.).
#' Passed closures must evaluate to a scalar numeric (double). Otherwise or if
#' an error is encountered, \code{NA_real_} will be returned.
#'
#' @details
#' The following objects are masked via a data-mask:
#' \describe{
#'   \item{.pred}{The predicted signal-values}
#'   \item{.obs}{The observed (original) signal-values}
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#' @importFrom purrr imap_dfr map2 map_dbl possibly
#' @importFrom rlang child_env enquos empty_env eval_tidy is_scalar_double
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export
mbte_compute_metrics <- function(x, ...) {
  assert_that(inherits(x, "tbl_mbte"))
  metrics <- enquos(...)

  # all elements of ellipsis must be named
  assert_that(!any(names(metrics) == ""),
    msg = "All elements of ellipsis must be named")

  # extract symbols from object
  value <- as.character(attr_value(x))
  signal <- as.character(attr_signal(x))
  fits <- as.character(attr_fits(x))

  eval_wrapper <- function(...) {
    res <- eval_tidy(...)
    if (!is_scalar_double(res)) {
      res <- NA_real_
    }
    res
  }
  # return NA_real_ if an error is encountered while evaluating a closure
  safe_eval <- possibly(eval_wrapper, NA_real_)

  # create masking environment for `.pred` and `.obs`
  mask <- child_env(empty_env())

  process_fit <- function(fit, name) {
    mask$.pred <- fit # predicted value of signal
    tibble(
      fit = name,
      metric = names(metrics),
      result = map_dbl(metrics, safe_eval, data = mask)
    )
  }

  create_metric_subtibble <- function(sig, all_fits) {
    mask$.obs <- sig[[value]] # value of original signal
    imap_dfr(all_fits, process_fit)
  }

  x %>%
    mutate(.tmp = map2(.[[!!signal]], .[[!!fits]], create_metric_subtibble)) %>%
    select(-!!signal, -!!fits) %>%
    unnest(.tmp)
}
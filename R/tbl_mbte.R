#' Creates a new dataset, suited for the the signal detection workflow
#'
#' A function intended to be used internally or to extend mbte.
#'
#' @param x A data.frame or tibble
#' @param time A symbol for the time-column (gets quoted). The corresponding
#' column must be present in the dataset.
#' @param value The value column (measurement-data); gets quoted; The
#' corresponding column must be present in the dataset.
#' @param signal A name for a list column, in which tibbles containing
#' the signal (time- and value- column combined) get stored; gets quoted
#' @param fits List-column-name; The predicted values for the original signal
#' get stored in this list-column.
#' @param metric The name for numeric-column containing the computed
#' error-metric.
#' @param initial Intended for the inital stage of the analysis - checks for
#' column-name integrity: `time` and `value` must be present, `signal`, `fits`
#' and `metric` must not be present.
#' @param ... Additional attributes, that are set.
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr is_scalar_logical
#' @importFrom rlang ensym
#' @importFrom tibble new_tibble
#' @export
new_tbl_mbte <- function(x, time, value, ..., signal = "signal", fits = "fits",
                         metric = "metric", subclass = NULL, initial = TRUE) {
  time <- ensym(time)
  value <- ensym(value)
  signal <- ensym(signal)
  fits <- ensym(fits)
  metric <- ensym(metric)

  # for the inital stage of the analysis only
  assert_that(is_scalar_logical(initial))
  if (initial) {
    assert_column_in_dataset(!!time, x, substitute(x))
    assert_column_in_dataset(!!value, x, substitute(x))
    assert_column_not_in_dataset(!!signal, x, substitute(x))
    assert_column_not_in_dataset(!!fits, x, substitute(x))
    assert_column_not_in_dataset(!!metric, x, substitute(x))
  }

  new_tibble(
    x,
    time = time,
    value = value,
    signal = signal,
    fits = fits,
    metric = metric,
    ...,
    subclass = c(subclass, "tbl_mbte")
  )
}
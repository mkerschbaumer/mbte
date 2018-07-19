#' Creates a new dataset, suited for the the signal detection workflow
#'
#' A function intended to be used internally or to extend mbte.
#'
#' @param x A data.frame or tibble
#' @param time A symbol for the time-column (gets quoted).
#' @param value The value column (measurement-data); gets quoted
#' @param signal A name for a list column, in which tibbles containing
#' the signal (time- and value- column combined) get stored; gets quoted
#' @param fits List-column-name; The predicted values for the original signal
#' get stored in this list-column.
#' @param metric The name for numeric-column containing the computed
#' error-metric.
#' @param ... Additional attributes, that are set.
#'
#' @importFrom purrr iwalk
#' @importFrom rlang ensym is_named
#' @importFrom tibble as_tibble is_tibble
#' @export
new_tbl_mbte <- function(x, time, value, ..., signal = "signal", fits = "fits",
                         metric = "metric", subclass = NULL) {
  time <- ensym(time)
  value <- ensym(value)
  signal <- ensym(signal)
  fits <- ensym(fits)
  metric <- ensym(metric)

  # convert input to tibble
  if (!is_tibble(x)) {
    x <- as_tibble(x)
  }

  # set attributes in ellipsis (make sure no names are missing)
  additional_args <- list(...)
  if (length(additional_args) != 0) {
    assert_that(is_named(additional_args))
    iwalk(list(...), ~{
      attr(x, .y) <<- .x
    })
  }

  attr_time(x) <- time
  attr_value(x) <- value
  attr_signal(x) <- signal
  attr_fits(x) <- fits
  attr_metric(x) <- metric

  class(x) <- c(subclass, "tbl_mbte", class(x))

  x
}
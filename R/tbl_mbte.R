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
#' @param subclass Additional classes to inherit from (character).
#' @param ... Additional attributes, that are set (every element must be named)
#'
#' @importFrom purrr iwalk
#' @importFrom rlang ensym
#' @importFrom tibble as_tibble is_tibble
#' @export
new_tbl_mbte <- function(x, time, value, ..., signal = "signal", fits = "fits",
                         subclass = NULL) {
  # convert input to tibble
  if (!is_tibble(x)) {
    x <- as_tibble(x)
  }

  # set attributes in ellipsis (make sure no names are missing)
  additional_args <- list(...)
  if (length(additional_args) != 0) {
    assert_ellipsis_named(additional_args, "(additional attributes to set)")
    iwalk(list(...), ~{
      attr(x, .y) <<- .x
    })
  }

  attr_time(x) <- ensym(time)
  attr_value(x) <- ensym(value)
  attr_signal(x) <- ensym(signal)
  attr_fits(x) <- ensym(fits)

  if (!missing(subclass)) {
    assert_is_character(subclass)
  }
  class(x) <- c(subclass, "tbl_mbte", class(x))

  x
}

#' Checks if an object is a \code{tbl_mbte}.
#'
#' @param x The object to check
#'
#' @details Currently, a valid \code{tbl_mbte} must be a tibble, which also
#' inherits from \code{tbl_mbte}. The attributes \code{time}, \code{value},
#' \code{signal} and \code{fits} have to be present.
#'
#' @importFrom rlang is_symbol
#' @importFrom tibble is_tibble
#' @export
is_tbl_mbte <- function(x) {
  # extract attribtues for checking
  time <- attr_time(x)
  value <- attr_value(x)
  signal <- attr_signal(x)
  fits <- attr_fits(x)

  # perform basic checks, including the correctness of the required attributes
  is_tibble(x) &&
    inherits(x, "tbl_mbte") &&
    is_symbol(time) &&
    is_symbol(value) &&
    is_symbol(signal) &&
    is_symbol(fits)
}

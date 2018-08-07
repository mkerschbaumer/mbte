#' The main data-structure of the mbte-package
#'
#' A \emph{tbl_mbte} is a \code{\link[tibble]{tibble}} under the hood.
#' Additionally, attributes are stored, which contain column-names. All
#' mentioned column names are stored as \code{\link[base:name]{symbols}}.
#'
#' @section Columns:
#' The following columns are used during the analysis (the attributes storing
#' the column names have the same names).
#' \describe{
#'   \item{time}{A numeric column containing the time of a specific
#'     measurement.}
#'   \item{value}{A numeric column containing the measured value of a signal
#'     (e.g. intensity).}
#'   \item{signal}{The time- and value-column combined into a list-column
#'     (e.g. by using \code{\link{mbte_nest_signals}}). Every element of this
#'     column is a \code{\link[tibble]{tibble}} containing the time- and
#'     value-column of the signal.}
#'   \item{fits}{This list-column also contains tibbles. The fitted
#'     signal-values are stored as columns. NOTE: Every contained tibble has the
#'     same number of rows as the signal-tibble of the same row. This column may
#'     be generated using \code{\link{mbte_fit}}.}
#' }
#'
#' @name tbl_mbte
#' @family tbl_mbte functions
NULL

#' Create a new \code{\link{tbl_mbte}}.
#'
#' A new \code{\link{tbl_mbte}} based on \code{x} gets created. The
#' corresponding column names for the `time`-, `value`-, `signal`- and
#' `fits` columns are passed as parameters as either strings or
#' \code{\link[base:name]{symbols}}. If symbols are passed,
#' \link[rlang]{quasiquotation} should be used.
#'
#' @note
#' Errors about missing or malformatted columns will not be rasised by this
#' function.
#'
#' @return
#' A \code{\link{tbl_mbte}} wrapping \code{x} will be returned. The resulting
#' object is a \code{\link[tibble]{tibble}} under the hood and can be used
#' as such.
#'
#' @param x A data.frame or tibble.
#' @param time The name of the time-column (gets quoted).
#' @param value The name of the value column (measurement-data) - gets quoted.
#' @param signal A name for a list column, in which tibbles containing
#'   the signal (time- and value- column combined) get stored - gets quoted.
#' @param fits List-column-name; The predicted values for the original signal
#'   get stored in this list-column - gets quoted.
#' @param subclass Additional classes to inherit from (character).
#' @param ... Additional attributes, that are set (must be named).
#'
#' @seealso \code{\link{raw_signals}} (dataset used in examples)
#' @examples
#' data(raw_signals)
#'
#' # rely on quotation
#' \dontrun{new_tbl_mbte(raw_signals, time = t, value = value)}
#'
#' # pass column names as strings
#' new_tbl_mbte(raw_signals, time = "t", value = "value")
#'
#' # pass symbols using quasiquotation
#' time_sym <- rlang::sym("t")
#' value_sym <- rlang::sym("value")
#' new_tbl_mbte(raw_signals, time = !!time_sym, value = !!value_sym)
#'
#' @importFrom purrr iwalk
#' @importFrom rlang ensym
#' @importFrom tibble as_tibble is_tibble
#' @family tbl_mbte functions
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

#' Checks if an object is a \code{\link{tbl_mbte}}.
#'
#' @param x The object to check.
#'
#' @details Currently, a valid \code{\link{tbl_mbte}} must be a
#' \code{\link[tibble]{tibble}}, which also inherits from `tbl_mbte`. The
#' attributes `time`, `value`, `signal` and `fits` have to be present and must
#' be \code{\link[base:name]{symbols}}.
#'
#' @seealso \code{\link{raw_signals}} (dataset used in examples)
#' @examples
#' data(raw_signals)
#'
#' # create new tbl_mbte
#' tbl <- new_tbl_mbte(raw_signals, time = "t", value = "value")
#'
#' \dontshow{stopifnot(is_tbl_mbte(tbl))}
#' is_tbl_mbte(tbl) # TRUE
#'
#' @importFrom rlang is_symbol
#' @importFrom tibble is_tibble
#' @family tbl_mbte functions
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

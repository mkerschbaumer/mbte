#' Column-name related functions
#'
#' Internal helper functions to access colnames used in a specific dataset;
#' Attributes are currently used to store column-names as symbols (to avoid
#' having a custom datastructure, since the dataset should still be tidyverse-
#' friendly)
#'
#' @param x A \code{\link{tbl_mbte}}.
#' @param value The name of the column as a \code{\link[base:name]{symbol}}.
#'
#' @return Replacement functions like \code{colname_time<-} return a modified
#'   \code{\link{tbl_mbte}}. Otherwise a \code{\link[base:name]{symbol}} is
#'   returned.
#'
#' @seealso \code{\link{tbl_mbte}} (explanation of used columns)
#' @name colname-functions
NULL

#' @describeIn colname-functions Get column name for signal-time.
#' @export
#' @keywords internal
colname_time <- function(x) {
  attr(x, "time")
}

#' @describeIn colname-functions Override column name for signal-time.
#' @export
#' @keywords internal
`colname_time<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "time") <- value
  x
}

#' @describeIn colname-functions Get column name for signal-values.
#' @export
#' @keywords internal
colname_value <- function(x) {
  attr(x, "value")
}

#' @describeIn colname-functions Set column name for signal-values column.
#' @export
#' @keywords internal
`colname_value<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "value") <- value
  x
}

#' @describeIn colname-functions Get column name for signal-column.
#' @export
#' @keywords internal
colname_signal <- function(x) {
  attr(x, "signal")
}

#' @describeIn colname-functions Override column name for signal-column.
#' @export
#' @keywords internal
`colname_signal<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "signal") <- value
  x
}

#' @describeIn colname-functions Get column name for fits-column (see
#'   \code{\link{mbte_fit}}).
#' @export
#' @keywords internal
colname_fits <- function(x) {
  attr(x, "fits")
}

#' @describeIn colname-functions Set column name for fits column.
#' @export
#' @keywords internal
`colname_fits<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "fits") <- value
  x
}


#' Column-name related functions
#'
#' Internal helper functions to access colnames used in a specific dataset;
#' Attributes are currently used to store column-names as symbols (to avoid
#' having a custom datastructure, since the dataset should still be tidyverse-
#' friendly)
#'
#' @param x A \code{\link{tbl_mbte}}.
#' @param value The name of the column as a \code{\link[base:name]{symbol}}.
#' @param name A character vector of length 1 (string) describing the purpose
#'   of the column to get/set (e.g. \code{"signal"} or \code{"fits"}).
#'
#' @return Replacement functions like \code{colname_time<-} return a modified
#'   \code{\link{tbl_mbte}}. Otherwise a \code{\link[base:name]{symbol}} is
#'   returned.
#'
#' @seealso \code{\link{tbl_mbte}} (explanation of used columns)
#' @name colname-functions
NULL

#' @describeIn colname-functions Set a specific column name. This function is
#'   designed for extension-packages.
#' @export
#' @keywords internal
colname_set <- function(x, value, name) {
  stopifnot(is.character(name), length(name) == 1, is_symbol(value))
  attr(x, name) <- value
  x
}

#' @describeIn colname-functions Get a specific column name. This function is
#'   designed for extension-packages.
#' @export
#' @keywords internal
colname_get <- function(x, name) {
  stopifnot(is.character(name), length(name) == 1)
  attr(x, name, exact = TRUE)
}

#' @describeIn colname-functions Get column name for signal-time.
#' @export
#' @keywords internal
colname_time <- function(x) {
  colname_get(x, "time")
}

#' @describeIn colname-functions Override column name for signal-time.
#' @export
#' @keywords internal
`colname_time<-` <- function(x, value) {
  colname_set(x, value, "time")
}

#' @describeIn colname-functions Get column name for signal-values.
#' @export
#' @keywords internal
colname_value <- function(x) {
  colname_get(x, "value")
}

#' @describeIn colname-functions Set column name for signal-values column.
#' @export
#' @keywords internal
`colname_value<-` <- function(x, value) {
  colname_set(x, value, "value")
}

#' @describeIn colname-functions Get column name for signal-column.
#' @export
#' @keywords internal
colname_signal <- function(x) {
  colname_get(x, "signal")
}

#' @describeIn colname-functions Override column name for signal-column.
#' @export
#' @keywords internal
`colname_signal<-` <- function(x, value) {
  colname_set(x, value, "signal")
}

#' @describeIn colname-functions Get column name for fits-column (see
#'   \code{\link{mbte_fit}}).
#' @export
#' @keywords internal
colname_fits <- function(x) {
  colname_get(x, "fits")
}

#' @describeIn colname-functions Set column name for fits column.
#' @export
#' @keywords internal
`colname_fits<-` <- function(x, value) {
  colname_set(x, value, "fits")
}


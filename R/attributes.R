# Helper functions to access attributes used across the mbte package;
# Attributes are used to store column-names as symbols in attributes (to avoid
# having a custom datastructure, since the dataset should still be tidyverse-
# friendly)

attr_time <- function(x) {
  attr(x, "time")
}

`attr_time<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "time") <- value
  x
}

attr_value <- function(x) {
  attr(x, "value")
}

`attr_value<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "value") <- value
  x
}

attr_signal <- function(x) {
  attr(x, "signal")
}

`attr_signal<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "signal") <- value
  x
}

attr_fits <- function(x) {
  attr(x, "fits")
}

`attr_fits<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "fits") <- value
  x
}

# event_log: a tibble with information about occurred unusual events and additional
# information
attr_event_log <- function(x) {
  attr(x, "event_log")
}

#' Retrieve logged information about occurred unusual events
#'
#' @param x A `tbl_mbte` with event-log information stored in attributes.
#'
#' @return A tibble with logged event-information. Each row represents an event.
#' @export
#
# NOTE: the purpose of this function is to export attr_event_log() but still
# keep the API of the mbte-package consistent (common prefix: mbte_)
mbte_event_log <- attr_event_log

`attr_event_log<-` <- function(x, value) {
  stopifnot(is_tibble(value))
  attr(x, "event_log") <- value
  x
}

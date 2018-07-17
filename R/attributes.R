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

attr_metric <- function(x) {
  attr(x, "metric")
}

`attr_metric<-` <- function(x, value) {
  stopifnot(is_symbol(value))
  attr(x, "metric") <- value
  x
}
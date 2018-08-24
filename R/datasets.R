# this file contains documentation of the datasets

#' Sample dataset of raw-signals
#'
#' A \code{\link[tibble]{tibble}} with randomly generated and exclusively
#' fictional signal-data. The intensities of fictional measurement variables are
#' measured every minute. The dataset contains 100 measurements of 42
#' measurement variables. A measurement variable can be thought of a variable
#' describing the measured parameter.
#'
#' @section Contained colums:
#' \describe{
#'   \item{t}{The time column.}
#'   \item{mv}{The measurement variable.}
#'   \item{value}{The measured intensity of the signal.}
#' }
#'
#' @name raw_signals
#' @family datasets
#' @docType data
#' @keywords data
NULL

#' Filtered raw-signals
#'
#' A \code{\link{tbl_mbte}} based on the \code{\link{raw_signals}}-dataset.
#' Signal-extraction has already been performed and short signals were removed.
#'
#' The following operations have been performed on \code{\link{raw_signals}}:
#' \itemize{
#'   \item conversion to a \code{\link{tbl_mbte}} via \code{\link{new_tbl_mbte}}
#'   \item nesting of `time`- and `value`-column via
#'     \code{\link{mbte_nest_signals}} (create `signal` list-column consisting
#'     of tibbles containing the time- and value-columns).
#'   \item extraction of subsignals via \code{\link{mbte_extract_subsignals}}
#'   \item filtering based on subsignal-length (only keep subsignals with a
#'     length greater than 20).
#' }
#'
#' @section Contained columns:
#' \describe{
#'   \item{mv}{The measurement variable. See \code{\link{raw_signals}} for
#'     details.}
#'   \item{signal_nr}{An integer column indicating the number of the subsignal
#'     within the original signal. See \code{\link{mbte_extract_subsignals}} for
#'     details.}
#'   \item{signal}{A list-column consisting of tibbles.}
#' }
#'
#' @name filtered_signals
#' @family datasets
#' @docType data
#' @keywords data
NULL
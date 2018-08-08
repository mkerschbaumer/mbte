#' Indexing functions for subsignal-extraction
#'
#' Indexing functions have the following signature:
#' \code{function(x, ...)}. \code{x} denotes a signal-values vector (see
#' \code{\link{tbl_mbte}} for  details). In other words, the measured values of
#' a signal get passed as a numeric vector. The job of an indexing function is
#' to find the start- and end-indices of a subsignal (used by
#' \code{\link{mbte_extract_subsignals}}).
#'
#' @note
#' The length of the returned indices must match the number of found subsignals
#' (e.g. 3 Subsignals found ==> length of start- and end-indices vectors must
#' equal 3). The indices-vectors must be of type integer.
#'
#' @return
#' A list with the following names should get returned:
#' \describe{
#'   \item{start}{Start indices of the found subsignals.}
#'   \item{end}{End indices of the found subsignals.}
#' }
#'
#' @name indexing-function
NULL

#' Extract subsignals from a \code{\link{tbl_mbte}}-object
#'
#' This function depends on an existing `signal`-column (see
#' \code{\link{tbl_mbte}}). The indexing function \code{f} is used to split
#' a signal into subsignals. This function is useful if only certaial parts
#' of a signal are relevant (e.g. remove parts, where the measured
#' signal-values are below a specific threshold via a custom
#' \code{\link{indexing-function}}).
#'
#' @param x A \code{\link{tbl_mbte}}-object.
#' @param f An \code{\link{indexing-function}}.
#' @param ... Additional arguments passed to `f`
#'
#' @section event-logging:
#' This function logs unusual events. A warning gets raised at the end of
#' execution, if events have been logged. The event-log can be retrieved
#' by passing the returned object to \code{\link{mbte_event_log}}. In this
#' case, a tibble containing the logged events will be returned to the user.
#'
#' @section event-log:
#' The tibble containing event-information consists of 3 columns:
#' \describe{
#'   \item{error}{The unprocessed error, which occurred during execution.}
#'   \item{row_nr}{The row-number of the input-tibble (\code{x}), at which the
#'     error occurred.}
#'   \item{signal}{The signal-subtable processed at the time the error occurred.}
#' }
#'
#' NOTE: currently warnings are not logged.
#'
#' @return The original table gets returned. The `signal`-column
#' is modified (since subsignals are extracted according to the indexing
#' function \code{f}). Additionally, the column `signal_nr` is added, which
#' indicates the number of the subsignal within the original signal.
#'
#' e.g. Assuming that \code{x} only contains one row (hence only one element is
#' present in the `signal`-list column). The indexing function \code{f}
#' determines, that the signal contains 3 subsignals. Therefore, the original
#' signal-tibble is split into 3 tibbles. The returned table will have 3 rows.
#' The column `signal_nr` will be equal to \code{c(1, 2, 3)}.
#'
#' @seealso \code{\link{raw_signals}} (dataset used in examples)
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data(raw_signals)
#'
#' # create nested tbl_mbte (needed for subsignal-extraction)
#' tbl <- raw_signals %>%
#'   group_by(mv) %>%
#'   new_tbl_mbte(time = "t", value = "value") %>%
#'   mbte_nest_signals()
#'
#' # a signal-subtable with leading zeros (should be removed)
#' tbl$signal[[9]]
#'
#' # perform subsignal extraction
#' #
#' # by default, mbte_default_indexer() gets used for signal-extraction
#' # in this case only nonzero values are interesting
#' extracted <- mbte_extract_subsignals(tbl)
#' extracted
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom purrr imap
#' @importFrom tidyr nest unnest
#' @export
mbte_extract_subsignals <- function(x, f = mbte_default_indexer, ...) {
  assert_is_tbl_mbte(x)
  assert_is_function(f)

  time <- attr_time(x)
  value <- attr_value(x)
  signals <- attr_signal(x)

  assert_has_column(x, signals, "(signal-column)")
  assert_valid_signal_col(x, signals)

  # only capture the first 10 errors
  event_store <- new_event_store(10L)

  # a wrapper around extract_subsignals(), which captures errors with the
  # corresponding row_number
  safe_extract_subsignals <- function(signal, row_nr, ...) {
    tryCatch(extract_subsignals(signal, ...), error = function(e) {
      event_store$add_event(error = e, row_nr = row_nr, signal = signal)
      tibble()
    })
  }

  x %>%
    mutate(!!signals := imap(!!signals, safe_extract_subsignals, indexer = f,
                            time = time, value = value, signals = signals, ...)
    ) %>%
    unnest(!!signals) %>%
    mbte_reconstruct(x) %>%
    # add error information (if errors occurred)
    cond_add_event_store(event_store, mbte_extract_subsignals)
}

#' The default \code{\link{indexing-function}} for the mbte-package
#'
#' A subsignal is defined as a sequence of nonzero values in \code{x}.
#' Therefore, elements near 0 are discarded and only the remaining nonzero
#' elements of \code{x} are kept.
#'
#' @param x A numeric vector (signal-values - see \code{\link{tbl_mbte}}).
#' @param ... Additional arguments (currently ignored)
#'
#' @examples
#' # create a dummy vector, simulating measurement data
#' # NOTE: the values of `x` are the same as their index
#' x <- c(1:4, 0, 6:8, 0, 0, 11:15)
#' x
#'
#' # NOTE: start indices: c(1, 6, 11)
#' # end indices: c(4, 8, 15)
#' indices <- mbte_default_indexer(x)
#' indices
#'
#' # show first subsignal:
#' x[indices$start[1]:indices$end[1]] # c(1, 2, 3, 4)
#'
#' @importFrom dplyr near
#' @export
mbte_default_indexer <- function(x, ...) {
  # add 0-padding (to ensure signal gets detected at the beginning and
  # at the end of the time series)
  padded_sig <- c(0, x, 0)

  has_signal <- as.integer(!near(padded_sig, 0))
  diff_signal <- diff(has_signal)

  # get positions of sub-signals (corresponding start and end)
  start <- which(diff_signal == 1)
  end <- which(diff_signal == -1) - 1L

  list(start = start, end = end)
}

# the actual workhorse-function for signal extraction
# x: signal-subtable
# indexer: indexing-function
# value_str: string containing the name of the value-column
# signals_sym: a symbol containing the name for the signal-column
#' @importFrom purrr map2
#' @importFrom rlang ":="
#' @importFrom tibble tibble
extract_subsignals <- function(x, indexer, time, value, signals, ...) {
  time_str <- as.character(time)
  value_str <- as.character(value)

  assert_has_column(x, time, "(time-column of subtibble)")
  assert_valid_time_col(x, time)
  assert_has_column(x, value, "(value-column of subtibble)")
  assert_valid_value_col(x, value)

  # extract indices of signal
  signal_ind <- indexer(x[[value_str]], ...)
  assert_is_list(signal_ind, "(returned object from indexing-function)")
  assert_is_integer(signal_ind$start, "(starting indices of subsignal)")
  assert_is_integer(signal_ind$end, "(end-indices of subsignal)")

  # extract subsignals via indices
  subsignals <- map2(signal_ind$start, signal_ind$end, ~{
    ind <- .x:.y # position of the signal
    tibble(!!time := x[[time_str]][ind], !!value := x[[value_str]][ind])
  })
  tibble(signal_nr = seq_along(subsignals), !!signals := subsignals)
}

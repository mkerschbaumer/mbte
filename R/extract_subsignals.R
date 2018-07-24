#' Extract subsignals from a `tbl_mbte`-object
#'
#' @param x A `tbl_mbte`-object
#' @param f An indexing function (a function taking a numeric vector and
#' returning a list). The returned list should contain the elements `start` and
#' `end` (interger-vector of the starting- or ending-positions of the
#' subsignal respecitvely).
#' @param ... Additional arguments passed to `f`
#'
#' @section event-logging:
#' This function logs unusual events. A warning gets raised at the end of
#' execution, if events have been logged. The error log can be retrieved
#' by passing the returned object to \code{\link{mbte_event_log}}.
#'
#' @section event-table:
#' A tibble containing event-information consists of 3 columns:
#' \describe{
#'   \item{error}{The unprocessed error, which occurred during execution.}
#'   \item{row_nr}{The row-number of the input-tibble (\code{x}), at which the
#'     error occurred.}
#'   \item{signal}{The signal-subtable processed at the time the error occurred.}
#' }
#'
#' @return The original table gets returned. The "signal"-column
#' is modified (since subsignals are extracted according to the indexing
#' function \code{f}).
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

#' The default indexer for the mbte-package
#'
#' @param x A numeric vector
#' @param ... Additional arguments (currently ignored)
#'
#' @importFrom assertthat assert_that
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

#' Extract subsignals from a `tbl_mbte`-object
#'
#' @param x A `tbl_mbte`-object
#' @param f An indexing function (a function taking a numeric vector and
#' returning a list). The returned list should contain the elements `start` and
#' `end` (interger-vector of the starting- or ending-positions of the
#' subsignal respecitvely).
#' @param ... Additional arguments passed to `f`
#'
#' @return The original table gets returned. The "signal"-column
#' is modified (since subsignals are extracted according to the indexing
#' function \code{f}).
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#' @export
mbte_extract_subsignals <- function(x, f = mbte_default_indexer, ...) {
  assert_that(inherits(x, "tbl_mbte"))
  time <- attr_time(x)
  value <- attr_value(x)
  signals <- attr_signal(x)

  x %>%
    mutate(!!signals := map(!!signals, extract_subsignals, indexer = f,
                            time = time, value = value, signals = signals, ...)
    ) %>%
    unnest(!!signals) %>%
    reconstruct(x)
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
  assert_that(!is.null(x), msg = "`x` must not be NULL")
  assert_that(is.numeric(x), msg = "`x` must be numeric or integer")
  assert_that(!(anyNA(x) || any(is.infinite(x) | is.nan(x))),
              msg = "`x` contains invalid elements")

  # add 0-padding (to ensure signal gets detected at the beginning and
  # at the end of the time series)
  padded_sig <- c(0, x, 0)

  has_signal <- as.integer(!near(padded_sig, 0))
  diff_signal <- diff(has_signal)

  # get positions of sub-signals (corresponding start and end)
  start <- which(diff_signal == 1)
  end <- which(diff_signal == -1) - 1

  list(start = start, end = end)
}

# the actual workhorse-function for signal extraction
# x: signal-subtable
# indexer: indexing-function
# value_str: string containing the name of the value-column
# signals_sym: a symbol containing the name for the signal-column
#' @importFrom purrr map2
#' @importFrom rlang ":=" eval_tidy quo
#' @importFrom tibble tibble
extract_subsignals <- function(x, indexer, time, value, signals, ...) {
  time_str <- as.character(time)
  value_str <- as.character(value)

  # extract indices of signal
  signal_ind <- indexer(x[[value_str]], ...)

  # extract subsignals via indices
  subsignals <- map2(signal_ind$start, signal_ind$end, ~{
    ind <- .x:.y # position of the signal
    tibble(!!time := x[[time_str]][ind], !!value := x[[value_str]][ind])
  })
  tibble(signal_nr = seq_along(subsignals), !!signals := subsignals)
}

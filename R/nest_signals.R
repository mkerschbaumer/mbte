#' Combine time- and variable- column to a list-column (signal).
#'
#' @param x A \code{tbl_mbte}, where the signals haven't been nested in a list
#' column yet (contains a time-, value- and a grouping column).
#' @param ... Variables (get quoted), which should be used for grouping. If no
#' variables are specified, \code{\link{mbte_nest_signals}} assumes, that
#' \code{x} is already a grouped table.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr group_by is_grouped_df
#' @importFrom tidyr nest
#' @importFrom rlang ensyms
#' @export
mbte_nest_signals <- function(x, ...) {
  assert_that(is_tbl_mbte(x))

  group_vars <- ensyms(...)
  if (length(group_vars) != 0) {
    # use grouping provided by ellipsis (higher priority than possibly
    # existing grouping of table)
    x <- x %>%
      group_by(!!!group_vars, add = FALSE) %>%
      mbte_reconstruct(x)
  } else {
    assert_that(is_grouped_df(x))
  }

  # extract relevant symbols
  time <- attr_time(x)
  value <- attr_value(x)
  signal <- attr_signal(x)

  # combine time- and value- variable into signal-list-column
  x %>%
    nest(!!time, !!value, .key = !!signal) %>%
    mbte_reconstruct(x)
}
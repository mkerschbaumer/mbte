#' Combine time- and variable- column to a list-column (signal).
#'
#' @param x A \code{tbl_mbte}, the time- and value-columns are still present (
#' specified when creating the `tbl_mbte`).
#' @param ... Variables (get quoted), which should be used for grouping. If no
#' variables are specified, \code{\link{mbte_nest_signals}} assumes, that
#' \code{x} is already a grouped table.
#'
#' @importFrom dplyr group_by
#' @importFrom purrr walk
#' @importFrom tidyr nest
#' @importFrom rlang ensyms
#' @export
mbte_nest_signals <- function(x, ...) {
  assert_is_tbl_mbte(x)

  group_vars <- ensyms(...)
  if (length(group_vars) != 0) {
    # make sure all grouping variables specified by the user are contained
    # in the dataset
    walk(group_vars, ~{
      assert_has_column(x, colname = .x, "(missing grouping-variable)")
    })

    # use grouping provided by ellipsis (higher priority than possibly
    # existing grouping of table)
    x <- x %>%
      group_by(!!!group_vars, add = FALSE) %>%
      mbte_reconstruct(x)
  } else {
    assert_grouped(x)
  }

  # extract relevant symbols
  time <- attr_time(x)
  value <- attr_value(x)
  signal <- attr_signal(x)

  assert_has_column(x, time, "(time-column)")
  assert_valid_time_col(x, time)
  assert_has_column(x, value, "(value-column)")
  assert_valid_value_col(x, value)

  # combine time- and value- columns into "signal"-list-column
  x %>%
    nest(!!time, !!value, .key = !!signal) %>%
    mbte_reconstruct(x)
}
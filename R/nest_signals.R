#' Combine `time`- and `value`- columns to a list-column (`signal`).
#'
#' This function is responsible for creating the `signal`-list-column of a
#' \code{\link{tbl_mbte}}. This gets done by combining the `time`- and
#' `value`-column of \code{x} via nesting (\code{\link[tidyr]{nest}} gets used).
#' This step is necessary, since functions like \code{\link{mbte_fit}} or
#' \code{\link{mbte_compute_metrics}} rely on the presence of the
#' `signal`-column. It is required to specify grouping columns (either by
#' passing them via \code{...} or by passing a
#' \code{\link[dplyr:grouped_df]{grouped table}} as the \code{x}-parameter).
#'
#' This function is meant to operate on `long`-datasets. Hence, columns for
#' signal-time, signal-values and a variable describing the measured parameter
#' should be present. Therefore, the "measurement-variable"-column should be
#' used for grouping if the goal is to find out, which measured parameters have
#' an underlying trend.
#'
#' @param x A \code{\link{tbl_mbte}}, which contains the `time`- and `value`-
#'   columns.
#' @param ... Variables (get \code{\link[rlang:quotation]{quoted}}), which
#'   should be used for grouping. If no variables are specified, it is assumed,
#'   that \code{x} is already a \code{\link[dplyr:grouped_df]{grouped table}}.
#'
#' @note
#' In any case, grouping variables specified in \code{...} are prioritised over
#' the grouping columns of a grouped table (if \code{x} is grouped).
#'
#' @seealso \code{\link{raw_signals}} (dataset used in examples)
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data(raw_signals)
#'
#' # create ungrouped tbl_mbte
#' tbl <- new_tbl_mbte(raw_signals, time = "t", value = "value")
#'
#' # NOTE: grouping variable specified manually (grouping based on the
#' # "measurement variable"-column, which is describing the measured parameter)
#' mbte_nest_signals(tbl, "mv")
#'
#' # alternative: use pregrouped table
#' tbl <- raw_signals %>%
#'   group_by(mv) %>%
#'   new_tbl_mbte(time = "t", value = "value")
#'
#' # yields the same result
#' mbte_nest_signals(tbl)
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
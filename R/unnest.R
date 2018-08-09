#' Perform unnesting of signals
#'
#' The `signal`-list column gets unnested. This function can be seen as the
#' inversion of \code{\link{mbte_nest_signals}}.
#'
#' @note
#' The `fits`-column will be removed, if it is present.
#'
#' @param x A \code{\link{tbl_mbte}}.
#'
#' @return
#' A \code{\link{tbl_mbte}} with the following columns:
#' \describe{
#'   \item{desc}{`desc` is a placeholder for all the columns except the `fits`-
#'     column. Those columns can be seen as descriptive columns.}
#'   \item{time}{The time column (originally in the `signal`-column).}
#'   \item{value}{The signal-values (before in the `signal`-list-column).}
#' }
#'
#' @seealso \code{\link{raw_signals}} (dataset used in examples)
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data(raw_signals)
#' raw_signals
#'
#' # create a tbl_mbte to nest
#' tbl <- raw_signals %>%
#'   group_by(mv) %>%
#'   new_tbl_mbte(time = "t", value = "value")
#'
#' # nest signals and unnest them (should be equal to `tbl`)
#' nested <- mbte_nest_signals(tbl)
#' unnested <- mbte_unnest_signals(nested)
#'
#' all.equal(unnested, tbl) # TRUE
#' \dontshow{stopifnot(isTRUE(all.equal(unnested, tbl)))}
#'
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @importFrom tidyr unnest
#' @family unnesting functions
#' @export
mbte_unnest_signals <- function(x) {
  assert_is_tbl_mbte(x)

  # extract column-names related symbols
  time <- attr_time(x)
  value <- attr_value(x)
  signal <- attr_signal(x)
  fits <- attr_fits(x)

  # check presence and integrity of signal-column
  assert_has_column(x, signal, "(signal-column)")
  assert_valid_signal_col(x, signal)

  # colnames for "descriptive" columns (not signal- or fits-columns) - needed
  # to reorder columns
  desc_cols <- setdiff(colnames(x), c(signal, fits))

  # perform unnesting
  x %>%
    unnest(!!signal) %>%
    select(!!!desc_cols, !!time, !!value) %>% # reorder columns
    mbte_reconstruct(x) # make sure a tbl_mbte gets returned
}

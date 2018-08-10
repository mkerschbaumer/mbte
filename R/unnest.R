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

#' Perform unnesting of fitting data
#'
#' The `fits`-list column gets unnested. This function is useful if the fitted
#' signal-values should be processed further.
#'
#' @return
#' A \code{\link{tbl_mbte}} with the following columns:
#' \describe{
#'   \item{desc}{`desc` is a placeholder for all columns except the `signal`-
#'     and the `fits`-columns. Those columns can be seen as "descriptive"
#'     columns describing the measured signals.}
#'   \item{fit}{The name of the method used for fitting (character). See
#'     \code{\link{mbte_fit}} for details.}
#'   \item{time}{The time-column unnested from the `signal`-column.}
#'   \item{value}{The values of the predicted signals (originally in `signal`-
#'     column).}
#' }
#'
#' @inheritParams mbte_unnest_signals
#'
#' @seealso \code{\link{filtered_signals}} (dataset used in examples)
#' @examples
#' # load sample dataset
#' data(filtered_signals)
#' filtered_signals
#'
#' # perform fitting
#' fitted <- mbte_fit(filtered_signals, lm = lm(value ~ t, .signal))
#'
#' # unnest predictions for signal-values (==> fitted signals)
#' #
#' # NOTE: "lm" is the only method used for fitting and therefore the `fit`-
#' # column consists only of "lm"
#' mbte_unnest_fits(fitted)
#'
#' @importFrom dplyr bind_cols mutate select
#' @importFrom magrittr "%>%"
#' @importFrom purrr pmap
#' @importFrom rlang expr
#' @importFrom tidyr gather unnest
#' @family unnesting functions
#' @export
mbte_unnest_fits <- function(x) {
  assert_is_tbl_mbte(x)

  # extract column-name related symbols
  time <- attr_time(x)
  value <- attr_value(x)
  signal <- attr_signal(x)
  fits <- attr_fits(x)

  # check integrity of columns
  assert_has_column(x, signal, "(signal-column)")
  assert_valid_signal_col(x, signal)
  assert_has_column(x, fits, "(fits-column)")
  assert_valid_fits_col(x, fits)

  # add time column to `fits` and convert actual fits to long-form (column
  # `fit` denotes the name of the fit)
  .modified_fits <- list(x[[fits]], x[[signal]], seq_len(nrow(x))) %>%
    pmap(function(fit, sig, ind) {
      # generate expression of the form x$signal[[row_number]] (for better
      # error-messages if assertions below fail)
      x_sym <- expr(`[[`(`$`(x, !!signal), !!ind))

      # check presence and integrity of time column
      assert_has_column(sig, time, x_sym = x_sym, "- signal subtibble faulty")
      assert_valid_time_col(sig, time, x_sym = x_sym)

      # add time column; NOTE: `fit` becomes the column containing the names of
      # the fits
      bind_cols(fit, select(sig, !!time)) %>%
        gather("fit", "value", -!!time)
    })

  # colnames of "descriptive" columns (columns not being signal- or
  # fits-columns and not `fit` - reserved column name)
  desc_cols <- setdiff(colnames(x), c(signal, fits, "fit"))

  x %>%
    # add time column to every table of `fits` list-column
    mutate(!!fits := .modified_fits) %>%
    unnest(!!fits) %>% # perform actual unnesting
    select(!!!desc_cols, "fit", !!time, !!value) %>% # reorder columns
    mbte_reconstruct(x) # make sure a tbl_mbte gets returned
}

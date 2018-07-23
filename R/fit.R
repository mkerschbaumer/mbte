#' Fit models to signals
#'
#' @param x A `tbl_mbte`, where signals have been extracted by
#' \code{\link{mbte_extract_subsignals}}.
#' @param ... It is assumed, that the ellipsis only contains named elements. The
#' elements are used as quosures and tidy evaluation is used. Caution:
#' \code{.signal}, \code{.time_sym} and \link{.value_sym} are masked (See
#' details for more information.).
#' Closures may return a numeric vector with the length of the original
#' signal-values or an object with a method for \code{\link[stats]{predict}}.
#' If the quosure evaluates to a numeric vector, it gets used as is. Otherwise,
#' \code{predict(returned_value, newdata = .signal)} will be called, in order to
#' get the predictions.
#'
#' @details
#' Quosures can make use of masked objects, that shadow their normal regular
#' execution environment:
#' \describe{
#'   \item{.signal}{A \code{\link[tibble]{tibble}} with the signal to fit
#'   (time and value are present)}
#'   \item{.time_sym}{The name of the time column as a
#'   \code{\link[base]{name}}.}
#'   \item{.value_sym}{The name of the value-column stored as a
#'   \code{\link[base]{name}}}
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate
#' @importFrom purrr map map_dfc
#' @importFrom rlang child_env empty_env enquos eval_tidy list2
#' @importFrom stats predict
#' @export
mbte_fit <- function(x, ...) {
  assert_that(inherits(x, "tbl_mbte"))
  fit_candidates <- enquos(...)
  assert_that(!any(names(fit_candidates) == ""),
    msg = "All elements of ellipsis must be named")

  signal <- attr_signal(x)
  fits <- attr_fits(x)

  # create data mask for tidy evaluation
  mask <- child_env(emptyenv(), .time_sym = attr_time(x), .value_sym = attr_value(x))

  fit_signal_impl <- function(sig) {
    map_dfc(fit_candidates, ~{
      # add signal tibble to masking-environment
      mask$.signal <- sig
      fit <- eval_tidy(.x, data = mask)
      # get predictions via predict(), if the quosure didn't evaluate to a numeric
      # vector
      if (!is.numeric(fit)) {
        fit <- predict(fit, newdata = sig)
      }

      # length of predicted value of the signal must match the row number of the
      # original signal table
      assert_that(identical(length(fit), nrow(sig)),
        msg = paste0("length of predicted signal doesen't match ",
          "length of the original signal-values")
      )

      # return fitted values
      fit
    })
  }

  x %>%
    mutate(!!fits := map(!!signal, fit_signal_impl)) %>%
    reconstruct(x)
}

# trend fitting helpers meant to be used with `mbte_fit()`

#' Trend-fitting helpers
#'
#' Helper functions meant to be used in conjunction with
#' \code{\link{mbte_fit}}.
#'
#' @return A fitting-\code{\link[rlang:quotation]{quosure}} will be returned.
#'
#' @seealso \code{\link{filtered_signals}} (dataset used in examples)
#' @examples
#' data(filtered_signals)
#'
#' # Use fitting helpers to fit signals
#' #
#' # NOTE: The unquoting operator (`!!`) is used, since fitting helpers return
#' # quosures.
#' fits1 <- mbte_fit(filtered_signals, lin = !!tr_linear())
#' fits1
#'
#' # Equivalent without using fitting-helpers (columns for time and value of
#' # signal have to be provided explicitly)
#' fits2 <- mbte_fit(filtered_signals, lin = lm(value ~ t, .signal))
#' fits2
#'
#' @name fitting-helpers
NULL

#' @describeIn fitting-helpers Determine a linear trend using
#'   \code{\link[stats]{lm}}.
#' @importFrom rlang new_formula quo
#' @importFrom stats lm
#' @export
tr_linear <- function() {
  quo({
    # formula used for fitting
    f <- new_formula(.value_sym, .time_sym)
    # fit linear model using fitting formula
    lm(f, .signal)
  })
}

#' @describeIn fitting-helpers The presence of an exponenetial trend is
#'   determined by applying a log-transformation to the signal-values and
#'   fitting them via \code{\link[stats]{lm}}. A vector of the correct length
#'   with \code{NA}'s will be returned if some values of the signal are zero or
#'   negative.
#' @importFrom rlang new_formula quo
#' @importFrom stats lm
#' @export
tr_exponential <- function() {
  quo({
    if (all(.signal[[.value_sym]] > 0)) {
      # perform log transformation
      .signal[[.value_sym]] <- log(.signal[[.value_sym]])

      # build fitting formula
      f <- new_formula(.value_sym, .time_sym)
      fit <- lm(f, .signal)

      # modify class (provide predict-method, that inverts the
      # log-transformation)
      class(fit) <- c("tr_exponential", class(fit))
      fit
    } else {
      rep(NA_real_, nrow(.signal))
    }
  })
}

#' @importFrom stats predict
#' @export
predict.tr_exponential <- function(object, newdata, ...) {
  predicted <- NextMethod()

  # invert log-transformation
  exp(predicted)
}

#' @describeIn fitting-helpers Fit a sigmoid model using
#'   \code{\link[nls2]{nls2}}. This fitting helper requires the `nls2`-package
#'   (the "port"-algorithm with 500 iterations is used). The signal-tibble to
#'   fit must have at least five rows (see \code{\link{mbte_fit}}). Otherwise
#'   \code{NA}'s are returned.
#'
#'   The formula used for fitting is
#'   \code{value ~ A / (1 + exp(B * (C - time))) + D}. NOTE: warnings are
#'   suppressed.
#' @importFrom rlang new_formula quo
#' @importFrom stats median nls.control quantile
#' @importFrom tibble tibble
#' @export
tr_logistic <- function() {
  if (!requireNamespace("nls2", quietly = TRUE)) {
    stop("The 'nls2'-package must be installed for sigmoid-fitting.")
  }

  quo({
    # for a simplified fitting formula: set `time` and `value` as the columns
    # for signal-time and signal-values.
    signal <- tibble(
      time = .signal[[.time_sym]],
      value = .signal[[.value_sym]]
    )

    # compute initial values for `A`, `C` and `D` parameters of fitting formula
    lower_part <- signal$value[signal$value <= quantile(signal$value, 0.2)]
    upper_part <- signal$value[signal$value >= quantile(signal$value, 0.8)]
    offset <- median(lower_part)
    asym <- median(upper_part) - offset
    # initialize `xmid` to the value closest to `value/2`
    xmid <- signal$time[which.min(abs(signal$value - (offset + asym / 2)))]

    nls_wrapper <- function() {
      # don't show error messages (regarding try()) + treat warnings as errors
      previous <- options(show.error.messages = FALSE)
      on.exit(options(previous))
      nls2::nls2(value ~ A / (1 + exp(B * (C - time))) + D, signal,
        algorithm = "port",
        control = nls.control(maxiter = 500, warnOnly = TRUE),
        start = expand.grid(
          A = asym, # asymptote
          B = seq(-1, 1, by = 0.25),
          C = xmid,
          D = offset
        )
      )
    }

    # Ensure at least five data points are present to perform a nls fit.
    if (nrow(signal) > 4) {
      # Perform fitting via nls2().
      fit <- suppressWarnings(nls_wrapper())

      # Return NA's if model doesen't converge.
      if (!fit$convInfo$isConv) {
        fit <- rep(NA_real_, nrow(signal))
      }
    } else {
      # less than five data points present for fitting
      fit <- rep(NA_real_, nrow(signal))
    }

    fit
  })
}

# variables provided by mbte_fit() via masking
globalVariables(c(".time_sym", ".value_sym", ".signal"))

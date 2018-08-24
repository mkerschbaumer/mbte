#' Log unusual events occurring during execution
#'
#' For some occasions (like in long-running computations) it may not be desired
#' to shut down the whole computation if a single error occurrs. It may be
#' better to log the occurred error and carry on with the computation. In such
#' a case, \code{\link{NA}} will always be the result of a failed computation.
#' The logged information can be retrieved via \code{mbte_event_log()} (see
#' examples).
#'
#' @seealso \code{\link{raw_signals}} (dataset in examples)
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data(raw_signals)
#' raw_signals
#'
#' # prepare for signal extraction (perform conversion to `tbl_mbte` and nest
#' # signals)
#' raw_signals <- raw_signals %>%
#'   group_by(mv) %>%
#'   new_tbl_mbte(time = "t", value = "value") %>%
#'   mbte_nest_signals()
#'
#' # provoke an error by passing an indexing-function, that will always raise
#' # an error
#' faulty_extract <- mbte_extract_subsignals(raw_signals, f = function(x) {
#'   stop("test")
#' })
#'
#' # retrieve event-log
#' event_log <- mbte_event_log(faulty_extract)
#' head(event_log)
#'
#' # show occurred errors
#' head(event_log$error)
#'
#' @name event-logging
NULL

# generate a list of functions with mutatble state:
#   + *add_event()*: add event with context
#     (e.g. add_event(error = err, input = bad_input))
#   + *get_events()*: return a tibble containing the events (each row being a
#     event, each column represents a name passed to ellipsis of add_event())
#   + *n_events()*: return the number of stored events
#
# threshold: if the number of elements added (e.g. errors) exceeds this
#   threshold, no new elements will be added and the generated object will act
#   as a noop.
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom rlang is_named is_scalar_integer
new_event_store <- function(threshold = Inf) {
  stopifnot(is_scalar_integer(threshold) || is.infinite(threshold))

  store <- list()
  funs <- list(
    add_event = function(...) {
      # only add events if the threshold doesen't get exceeded
      if ((length(store) + 1) <= threshold) {
        elements <- list(...)
        stopifnot(is_named(elements))
        # wrap non-scalar elements in a list
        elements <- map(elements, wrap_nonscalar)
        store <<- append(store, list(elements))
      }
      invisible(NULL)
    },
    get_events = function() {
      # return a tibble with each event being a row
      bind_rows(store)
    },
    n_events = function() {
      # return number of recorded events
      length(store)
    }
  )

  # add custom class
  class(funs) <- c("mbte_event_log", class(funs))
  funs
}

# wrap `x` in a list if it is not a scalar vector (enhances readability of
# event-log printing, since e.g. scalar integers denoting row names where an
# event occurred won't get wrapped)
#' @importFrom rlang is_scalar_vector
wrap_nonscalar <- function(x) {
  if (is_scalar_vector(x)) {
    x
  } else {
    list(x)
  }
}

# conditionally add event-log tbl to a `tbl_mbte` (if entries are present in
# event store)
#
# NOTE: caller_fn represents the caller function (gets used as symbol for a
# warning message, if unusual events have been recorded during its execution)
#' @importFrom rlang ensym
cond_add_event_store <- function(x, store, caller_fn) {
  assert_class(store, "mbte_event_log")
  caller_fn <- ensym(caller_fn)

  if (store$n_events() != 0) {
    attr_event_log(x) <- store$get_events()
    warning(
      "Unusual events occurred during execution of `",
      as.character(caller_fn), "()` ==> can be retrieved via mbte_event_log()"
    )
  }

  x
}

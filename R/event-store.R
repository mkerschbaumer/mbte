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
        elements <- map(elements, list) # wrap every passed element in a list
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
    warning("Unusual events occurred during execution of `",
      as.character(caller_fn), "()` ==> can be retrieved via mbte_event_log()")
  }

  x
}
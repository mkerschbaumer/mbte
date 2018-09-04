# provide a counter with mutable state
new_counter <- function(initial_value = 0L) {
  counter <- initial_value

  list(
    increment = function(by = 1L) {
      counter <<- counter + by
    },
    decrement = function(by = 1L) {
      counter <<- counter - by
    },
    value = function() {
      counter
    }
  )
}

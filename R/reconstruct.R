#' Reconstruct a \code{\link{tbl_mbte}} from template-object
#'
#' This function should be used to readd dropped-attributes, if a
#' \code{\link{tbl_mbte}}-object gets processed by functions, which may drop
#' additional attributes.
#'
#' The implementation of \code{sloop::reconstruct()} gets used; This is a
#' temporary workaround and will be removed, once the
#' \href{https://github.com/hadley/sloop}{sloop}-package is on CRAN.
#'
#' @param new Object, to which the attributes should be added (must be
#'   convertible to a tibble).
#' @param old \code{\link{tbl_mbte}}, from which specific attributes are
#'   copied (`time`, `value`, `signal`, `fits`). Additionally, the class
#'   `tbl_mbte` gets added. The S3-generic dispatches on this argument.
#'
#' @seealso \code{\link{raw_signals}} (dataset used in examples)
#' @examples
#' data(raw_signals)
#'
#' # create template object for attribute reconstruction
#' template <- new_tbl_mbte(raw_signals, time = "t", value = "value")
#'
#' # create target object, which lacks of the "time"-attribute
#' target <- new_tbl_mbte(raw_signals, time = "t", value = "value")
#' attr(target, "time") <- NULL
#'
#' \dontshow{stopifnot(!is_tbl_mbte(target))}
#' is_tbl_mbte(target) # FALSE
#'
#' # reconstruct valid tbl_mbte from template
#' target <- mbte_reconstruct(target, template)
#'
#' \dontshow{stopifnot(is_tbl_mbte(target))}
#' is_tbl_mbte(target) # TRUE
#'
#' @include tbl_mbte.R
#' @family tbl_mbte functions
#' @export
mbte_reconstruct <- function(new, old) {
  UseMethod("mbte_reconstruct", old)
}

#' @rdname mbte_reconstruct
#' @export
mbte_reconstruct.tbl_mbte <- function(new, old) {
  assert_is_tbl_mbte(old)
  new_tbl_mbte(
    new,
    time = !!attr_time(old),
    value = !!attr_value(old),
    signal = !!attr_signal(old),
    fits = !!attr_fits(old)
  )
}
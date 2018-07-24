#' Reconstruct S3-tbl_mbte-class from object
#'
#' This function should be used to readd dropped-attributes, if a
#' `tbl_mbte`-object gets processed by functions, which may drop additional
#' attributes. The mbte-package relies on attributes in order to store
#' information about column-names (e.g. name of the time-column) via attributes.
#'
#' @param new new object
#' @param old Object, from which specific attributes are copied (time, value,
#' signal, fits, metric). The S3-generic dispatches on this argument.
#'
#' The implementation of `sloop::reconstruct()` gets used; This is a temporary
#' workaround and will be removed, once the sloop-package is on CRAN.
#'
#' @export
mbte_reconstruct <- function(new, old) {
  UseMethod("mbte_reconstruct", old)
}

#' @export
mbte_reconstruct.tbl_mbte <- function(new, old) {
  assert_is_tbl_mbte(old)
  new_tbl_mbte(
    new,
    time = !!attr_time(old),
    value = !!attr_value(old),
    signal = !!attr_signal(old),
    fits = !!attr_fits(old),
    metric = !!attr_metric(old)
  )
}
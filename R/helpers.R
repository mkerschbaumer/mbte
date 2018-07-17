# throws an error, if the specified column is not in the dataset
#' @importFrom assertthat assert_that
#' @importFrom rlang ensym eval_tidy expr expr_label
assert_column <- function(contained) {
  function(name, x, dataset_name) {
    name <- ensym(name)
    e <- expr(as.character(name) %in% colnames(x))
    if (!contained) {
      e <- expr(!(!!e))
    }
    negation <- ifelse(contained, "", "not ")
    assert_that(eval_tidy(e),
                msg = paste0(expr_label(name), " must ", negation,
                             "be a column in ", expr_label(dataset_name))
    )
  }
}

assert_column_in_dataset <- assert_column(TRUE)
assert_column_not_in_dataset <- assert_column(FALSE)

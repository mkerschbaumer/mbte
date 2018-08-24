# precompute a tbl_mbte based on the `raw_signals` dataset
gen_raw_tbl_mbte <- local({
  precomputed <- new_tbl_mbte(raw_signals, time = t, value = value)

  function() {
    precomputed
  }
})

# generate a grouped tbl_mbte based on the `raw_signals`-dataset
gen_raw_gr_tbl_mbte <- local({
  precomputed <- new_tbl_mbte(dplyr::group_by(raw_signals, mv), t, value)
  stopifnot(dplyr::is_grouped_df(precomputed))

  function() {
    precomputed
  }
})

# generate extended tbl_mbte - add a simulated column to it (may also be used
# as a grouping-column)
gen_ext_tbl_mbte <- local({
  raw_tbl <- gen_raw_tbl_mbte()

  # extend table by adding a simulated column (ensure reproducibility)
  withr::with_seed(42L, {
    ext_tbl <- dplyr::mutate(raw_tbl,
      extra_column = sample(letters[1:6], nrow(raw_tbl), replace = TRUE)
    )
  })
  # make sure resulting object is a tbl_mbte
  ext_tbl <- mbte_reconstruct(ext_tbl, raw_tbl)

  function() {
    ext_tbl
  }
})

# testing helper for mbte_compute_metrics(): simulate cases, where it is known,
# that every computed result will be equal to `result`. `metric_names` contains
# the names of the metric-quosures, which produce the passed result.
#
# NOTE: `dataset` should be a tibble containing the `fits` column
gen_metric_result <- function(dataset, result, metric_names, signal, fits) {
  # column names for signal- and fits column
  signal <- rlang::ensym(signal)
  fits <- rlang::ensym(fits)

  dataset %>%
    dplyr::mutate(.tmp = purrr::map(!!fits, ~{
      purrr::cross_df(list(
        # names of the fits, which are present in passed dataset (e.g. "loess")
        fit = colnames(.x),
        metric = metric_names,
        result = result
      ))
    })) %>%
    dplyr::select(-!!signal, -!!fits) %>% # remove unneeded columns
    tidyr::unnest(.tmp)
}

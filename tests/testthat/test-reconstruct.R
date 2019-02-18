context("reconstruct")

# create temporary datasets for testing
# without grouping
basic_df <- as.data.frame(raw_signals)
basic_tbl <- raw_signals # tibble
basic_tbl_mbte <- gen_raw_tbl_mbte()
# with grouping
gr_df <- dplyr::group_by(basic_df, mv)
gr_tbl <- dplyr::group_by(basic_tbl, mv)
gr_tbl_mbte <- gen_raw_gr_tbl_mbte()

# independent dataset, which serves as a "template" for reconstruction (contains
# the attribtutes, that should be set for the target object).
reference <- new_tbl_mbte(raw_signals, t, v, signal = s, fits = f)

# assert attributes are set correctly and that class attribute ("tbl_mbte") gets
# assigned
is_valid_reconstruction <- function(obj) {
  expect_true(tibble::is_tibble(obj))
  expect_true(is_tbl_mbte(obj))

  # Test if colnames (to use in analysis) have been set correctly.
  expect_identical(colname_time(obj), colname_time(reference))
  expect_identical(colname_value(obj), colname_value(reference))
  expect_identical(colname_signal(obj), colname_signal(reference))
  expect_identical(colname_fits(obj), colname_fits(reference))
}

# a wrapper around is_valid_reconstruction(), that additionally checks, if the
# grouping variables (dplyr) have been preserved.
is_valid_gr_reconstruction <- function(obj, gr_vars = "mv") {
  is_valid_reconstruction(obj)
  expect_identical(dplyr::group_vars(obj), gr_vars)
}

# test to run
tests <- tibble::tribble(
  ~name, ~input, ~check_fun,
  "basic data.frame", basic_df, is_valid_reconstruction,
  "basic tibble", basic_tbl, is_valid_reconstruction,
  "basic `tbl_mbte`", basic_tbl_mbte, is_valid_reconstruction,
  # the following grouping tests ensure, that the grouping attributes from dplyr
  # should be preserved as they are (must stay untouched by reconstruct())
  "grouped data.frame", gr_df, is_valid_gr_reconstruction,
  "grouped tibble", gr_tbl, is_valid_gr_reconstruction,
  "grouped `tbl_mbte`", gr_tbl_mbte, is_valid_gr_reconstruction
)

# run tests
purrr::pmap(tests, function(name, input, check_fun) {
  test_that(name, {
    res <- mbte_reconstruct(input, reference)
    # make sure reconstruction is valid
    check_fun(res)
  })
})

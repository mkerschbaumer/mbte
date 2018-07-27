context("reconstruct")

# object, which serves as a "template" for reconstruction (contains the
# attribtutes, that should be set for the target object).
withr::with_seed(testing_seed(), {
  # "Species" chosen for the prefix of dummy measurement variable (for easier
  # testing with `is_valid_gr_reconstruction()`)
  reference <- gen_raw_tbl_mbte(mv_prefix = "Species")
})

# assert attributes are set correctly and that class attribute ("tbl_mbte") gets
# assigned
is_valid_reconstruction <- function(obj) {
  expect_true(is_tibble(obj))
  expect_true(is_tbl_mbte(obj))

  # test if attributes have been set correctly
  expect_identical(attr_time(obj), attr_time(reference))
  expect_identical(attr_value(obj), attr_value(reference))
  expect_identical(attr_signal(obj), attr_signal(reference))
  expect_identical(attr_fits(obj), attr_fits(reference))
  expect_identical(attr_metric(obj), attr_metric(reference))
}

# a wrapper around is_valid_reconstruction(), that additionally checks, if the
# grouping variables (dplyr) have been preserved.
is_valid_gr_reconstruction <- function(obj, gr_vars = "Species") {
  is_valid_reconstruction(obj)
  expect_identical(dplyr::group_vars(obj), gr_vars)
}

# create temporary datasets for testing
iris_tbl <- as_tibble(iris)
withr::with_seed(testing_seed(), {
  basic_tbl_mbte <- gen_raw_tbl_mbte(mv_prefix = "Species")
})
iris_gr <- group_by(iris, Species)
iris_tbl_gr <- group_by(iris_tbl, Species)
basic_tbl_mbte_gr <- group_by(basic_tbl_mbte, Species)

tests <- tibble::tribble(
  ~name, ~input, ~check_fun,
  "basic data.frame", iris, is_valid_reconstruction,
  "basic tibble", iris_tbl, is_valid_reconstruction,
  "basic `tbl_mbte`", basic_tbl_mbte, is_valid_reconstruction,
  # the following grouping tests ensure, that the grouping attributes from dplyr
  # should be preserved as they are (must stay untouched by reconstruct())
  "grouped data.frame", iris_gr, is_valid_gr_reconstruction,
  "grouped tibble", iris_tbl_gr, is_valid_gr_reconstruction,
  "grouped `tbl_mbte`", basic_tbl_mbte_gr, is_valid_gr_reconstruction
)

purrr::pmap(tests, function(name, input, check_fun) {
  test_that(name, {
    res <- mbte_reconstruct(input, reference)
    check_fun(res)
  })
})

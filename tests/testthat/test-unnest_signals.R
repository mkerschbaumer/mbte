context("unnest_signals")

raw_tbl_mbte <- new_tbl_mbte(raw_signals, "t", "value")

test_input_not_tbl_mbte(mbte_unnest_signals)

# test signal column not present or malformatted
test_signal_col_np_mf(mbte_unnest_signals, raw_tbl_mbte)

# mbte_unnest_signals() should invert mbte_nest_signals()
test_that("inversion of mbte_nest_signals", {
  expect_silent(nested <- mbte_nest_signals(raw_tbl_mbte, mv))
  expect_silent(unnested <- mbte_unnest_signals(nested))

  # reorder columns (since mbte_unnest_signals() always puts time- and value-
  # columns after "descriptive" columns like `mv`)
  reordered <- select(raw_tbl_mbte, mv, dplyr::everything())

  # make sure inversion has been successful
  expect_tbl_mbte_equal(unnested, reordered)
})

test_that("positive test with `fits`-column", {
  # add `fits`-column
  fitted <- filtered_signals %>%
    mbte_fit(lm = lm(value ~ t, .signal))

  # unnesting of tbl_mbte with `fits`-column should yield the same results as
  # unnesting a tbl_mbte without `fits`-column
  fitted_unnested <- mbte_unnest_signals(fitted)
  original_unnested <- mbte_unnest_signals(filtered_signals)
  expect_tbl_mbte_equal(fitted_unnested, original_unnested)
})
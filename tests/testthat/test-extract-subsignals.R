context("extract_subsignals")

# global nested table (to avoid recomputation)
# NOTE: "signal_var" is be the name of the signal-variable, that should be
# used for subsignal-extraction
tbl_nested <- gen_raw_gr_tbl_mbte() %>%
  mbte_nest_signals() %>%
  dplyr::rename(signal_var = signal) %>%
  new_tbl_mbte(t, value, signal = signal_var)

# `x` is not a tbl_mbte
test_input_not_tbl_mbte(mbte_extract_subsignals)

test_that("indexing function is not a function", {
  expect_error(mbte_extract_subsignals(tbl_nested, "abc"),
    class = "err_class_mismatch",
    regexp = "not.+function"
  )
})

# test signal column not present or malformatted
test_signal_col_np_mf(mbte_extract_subsignals, gen_raw_tbl_mbte())

# subtibble with malformatted time column contained (character instead of
# numeric)
test_malformatted_signal_subtable(mbte_extract_subsignals, tbl_nested,
  target_col = "t",
  signal = "signal_var"
)

# subtibble with malformatted value column
test_malformatted_signal_subtable(mbte_extract_subsignals, tbl_nested,
  target_col = "value",
  signal = "signal_var"
)

test_that("user-provided indexing function raises error", {
  ind_fun <- function(x, ...) {
    e <- simpleError("custom error")
    class(e) <- c("custom_error_class", class(e))
    stop(e)
  }

  with_event_log(
    mbte_extract_subsignals(tbl_nested, ind_fun),
    err_check = create_err_checker("custom error", "custom_error_class")
  )
})

test_that("ellipsis forwarded to indexing function", {
  custom_ind_fun <- function(x, ...) {
    ellipsis <- list(...)

    expect_equal(ellipsis$test_num, 42)
    expect_equal(ellipsis$test_str, "test")

    mbte_default_indexer(x, ...)
  }

  # mbte_extract_subsignals() should record errors occurring during execution
  # in error log; expect_equal() may raise errors about failed assertions -
  # it is assumed, that no assertion fails ==> no warning should be raised
  expect_silent(
    mbte_extract_subsignals(tbl_nested, custom_ind_fun,
      test_num = 42, # pass additional parameters
      test_str = "test"
    )
  )
})

# wrong "return type" of indexing function
test_that("indexing function doesen't return a list", {
  ind_fun <- function(x, ...) {
    1:10 # should return a list but returns a numeric vector
  }

  with_event_log(
    mbte_extract_subsignals(tbl_nested, ind_fun),
    err_check = err_class_mismatch_checker("not.+list.+indexing.+function")
  )
})

# malformat a component of the returned list from the indexing function
# (either "start" or "end"-indices - convert them to character ==> invalid)
malformat_return <- function(which) {
  stopifnot(which %in% c("start", "end"))
  function(x, ...) {
    ind_lst <- mbte_default_indexer(x, ...)
    ind_lst[[which]] <- as.character(ind_lst[[which]])

    ind_lst
  }
}

# malformatted start indices (of subsignal)
test_that("indexing function - start indices not integers", {
  ind_fun <- malformat_return("start")

  with_event_log(
    mbte_extract_subsignals(tbl_nested, ind_fun),
    err_check = err_class_mismatch_checker("ind\\$start.+not.+integer")
  )
})


# malformatted end indices
test_that("indexing function - end indices not integers", {
  ind_fun <- malformat_return("end")

  with_event_log(
    mbte_extract_subsignals(tbl_nested, ind_fun),
    err_check = err_class_mismatch_checker("ind\\$end.+not.+integer")
  )
})

test_that("positive test defaultIndexer", {
  res <- tbl_nested %>%
    # create list column with returned values from mbte_default_indexer()
    dplyr::mutate(positions = purrr::map(signal_var, ~{
      # invoke default indexer with signal-values as input
      ind <- mbte_default_indexer(.x$value)
      tibble::tibble(
        signal_nr = seq_along(ind$start),
        start = ind$start,
        end = ind$end
      )
    })) %>%
    # signal column not needed anymore
    dplyr::select(-signal_var) %>%
    tidyr::unnest(positions) %>%
    # extract column-number from mv-variable (simulated measurement-variable)
    # NOTE: column-number refers to simulated mesasurement variables
    dplyr::mutate(col = as.integer(substring(mv, 3))) %>%
    # reorder columns
    dplyr::select(col, signal_nr, start, end)

  # expected result
  exp <- dplyr::select(sig_tbl, -signal)

  expect_equal(res, exp)
})

test_that("positive test mbte_extract_subsignals()", {
  # compute expected result
  exp <- sig_tbl %>%
    # start and end indices irrelevant
    dplyr::select(-start, -end) %>%
    dplyr::rename(signal_var = signal) %>%
    dplyr::mutate(
      mv = paste0("mv", col), # create mv column
      # change name of time column in every signal-subtibble (for consistency
      # with `tbl_nested`)
      signal_var = map(signal_var, ~{
        dplyr::rename(.x, t = time)
      })
    ) %>%
    # only keep relevant columns and convert to tbl_mbte
    dplyr::select(mv, signal_nr, signal_var) %>%
    new_tbl_mbte(t, value, signal = signal_var)

  # compute actual result
  res <- mbte_extract_subsignals(tbl_nested)

  # perform deep comparison between the actual and the expected result
  expect_tbl_mbte_equal(res, exp)
})

context("extract_subsignals")

# global tables (to avoid recomputation)
withr::with_seed(testing_seed(), {
  # grouped with raw data (needed because it doesen't have the signal-column)
  tbl <- gen_raw_gr_tbl_mbte()
  # nested
  tbl_nested <- mbte_nest_signals(tbl)
})

# `x` is not a tbl_mbte
test_input_not_tbl_mbte(mbte_extract_subsignals)

test_that("indexing function is not a function", {
  expect_error(mbte_extract_subsignals(tbl, "abc"),
    class = "err_class_mismatch", regexp = "not.+function")
})

# test signal column not present or malformatted
test_signal_col_np_mf(mbte_extract_subsignals, tbl)

# subtibble with malformatted time column contained (character instead of
# numeric)
test_malformatted_signal_subtable(mbte_extract_subsignals, tbl_nested, "time")

# subtibble with malformatted value column
test_malformatted_signal_subtable(mbte_extract_subsignals, tbl_nested, "value")

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
    mbte_extract_subsignals(tbl_nested, custom_ind_fun, test_num = 42,
    test_str = "test")
  )
})

# wrong "return type" of indexing function
test_that("indexing function doesen't return a list", {
  ind_fun <- function(x, ...) {
    x # should return a list but returns a numeric vector
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


# generate global variables for positive tests (no errors expected)
withr::with_seed(testing_seed(), {
  # generate random column names
  time <- gen_random_sym()
  value <- gen_random_sym()
  signal <- gen_random_sym()

  # generate intermediate signals-table, which contains position-indices
  signals <- gen_sim_ind(nrow = 100L, ncol = 42L, max_subsig = 3L) %>%
    gen_sim_sig_default()

  # generate raw dataset (add 0-padding between signals accordingly) and nest
  # signals (combine time- and value-column to signal-list-column)
  nested_dataset <- signals %>%
    gen_sim_raw(mv_prefix = "mv") %>%
    dplyr::rename(!!time := time, !!value := value) %>%
    dplyr::group_by(mv) %>%
    new_tbl_mbte(time = !!time, value = !!value, signal = !!signal) %>%
    mbte_nest_signals()
})

test_that("positive test defaultIndexer", {
  res <- nested_dataset %>%
    # create list column with returned values from mbte_default_indexer()
    dplyr::mutate(positions = purrr::map(!!signal, ~{
      # invoke default indexer with signal-values as input
      ind <- mbte_default_indexer(.x[[as.character(value)]])
      tibble::tibble(signal_nr = seq_along(ind$start), start = ind$start,
        end = ind$end)
    })) %>%
    # remove unneeded signal column
    dplyr::select(-!!signal) %>%
    tidyr::unnest(positions) %>%
    # extract column-number from mv-variable (simulated measurement-variable)
    # NOTE: column-number refers to simulated mesasurement variables
    dplyr::mutate(col = as.integer(substring(mv, 3))) %>%
    # change column order
    dplyr::select(col, signal_nr, start, end)

  # expected result
  exp <- dplyr::select(signals, -signal)

  expect_equal(res, exp)
})

test_that("positive test mbte_extract_subsignals()", {
  # compute expected result
  exp <- signals %>%
    # start and end indices irrelevant
    dplyr::select(-start, -end) %>%
    # rename time and value-column in every signal-subtibble
    dplyr::mutate(signal = purrr::map(signal, ~{
      dplyr::rename(.x, !!time := time, !!value := value)
    })) %>%
    # change column name of signal column
    dplyr::rename(!!signal := signal) %>%
    # create mv column
    dplyr::mutate(mv = paste0("mv", col)) %>%
    # only keep relevant columns for comparison and create tbl_mbte
    dplyr::select(mv, signal_nr, !!signal) %>%
    new_tbl_mbte(!!time, !!value, signal = !!signal)

  # actual result
  res <- mbte_extract_subsignals(nested_dataset)

  # perform deep comparison between the actual and the expected result
  expect_tbl_mbte_equal(res, exp)
})

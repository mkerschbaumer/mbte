# generate a index table with the following structure: (all integer columns)
# | col (number of additional column) | signal_nr | start | end |
# `start` and `end` are the starting and ending indices of the corresponding
# subsignal
#
# NOTE: every row is a subsignal
gen_sim_ind <- function(nrow, ncol, max_subsig) {
  stopifnot(is.integer(nrow), is.integer(ncol), is.integer(max_subsig))

  # to choose number of subsignals in signal randomly
  choices_n_subsig <- seq_len(max_subsig)
  # subsignal positions (both start and end) to sample from
  choices_subsig_ind <- seq_len(nrow)

  purrr::map_dfr(seq_len(ncol - 1), ~{
    # sample number of subsignals
    n_subsig <- sample(choices_n_subsig, 1)

    # odd positions of `indices` represent starting indices, even ones stand
    # for ending indices of the subsignal; keep sampling until all differences
    # of the indices are greater than 1 (ensures subsignals have at least one 0
    # between them)
    repeat({
      indices <- sort(sample(choices_subsig_ind, 2 * n_subsig, replace = FALSE))
      if (all(diff(indices) > 1)) {
        break
      }
    })

    is_start <- as.logical((seq_along(indices) %% 2) == 1)

    tibble::tibble(col = .x, signal_nr = seq_len(n_subsig), start = indices[is_start],
      end = indices[!is_start])
  }) %>%
    # ensure the range of [1, nrow] (otherwise it's not guaranteed that 1 is
    # the minimum of starting indices and `nrow` likewise not the maximum)
    tibble::add_row(col = ncol, signal_nr = 1L, start = 1L, end = nrow)
}

# helper-functions for `gen_sim_sig` to generate randomized coefficients
coef_funs <- list(
  # generate a random coefficient withing the range [min, max]
  # NOTE: the coefficient gets sampled from the speciefied interval with 50
  # subdivisions
  r_coef = function(min, max) {
    sample(seq(min, max, length.out = 50L), 1L)
  },
  # same as r_coef(), but sample from a symmetric interval (-value, value)
  rs_coef = function(value) {
    abs_value <- abs(value)
    coef_funs$r_coef(-abs_value, abs_value)
  }
)

# generate simulated signal: take the output of `gen_sim_ind()` as first
# argument and add the signal-column (list column with columns `time` and
# `value`).
#
# The elements of the ellipsis get used as quosures, which should generate
# signal-values ("generators"). The following symbols are masked:
#
# + n: The length of the subsignal to generate
# + x: equivalent to seq_len(n) - can be interpreted as a simulated "time"
# + c_x: x centered around 0
# + helper functions from coef_funs (currently r_coef() and rs_coef())
#   ==> sample randomized coefficients from a specified interval
#
# `noise_fun` is a function, which is used to add noise to the generated
# signals. It gets invoked with the first element being the number of random
# values to generate. Additionally, sd gets passed (standard deviation of the
# subsignal).
#
# `weights` is a numeric vector for sampling probabilities (for the quosures
# passed to `...`)
gen_sim_sig <- function(ind_tbl, ..., noise_fun = rnorm, weights = NULL) {
  stopifnot(tibble::is_tibble(ind_tbl))

  generator_quos <- rlang::enquos(...)
  stopifnot(length(generator_quos) != 0)

  # to choose a random generator
  choices_gen <- seq_along(generator_quos)

  # amount of noise to add (multiplied by the output of `noise_fun`)
  choices_noise_amount <- seq(0.1, 0.8, 0.1)

  # masking environment for tidy evalutation
  mask <- rlang::new_environment(coef_funs)

  if (missing(weights)) {
    # assume the user wanted equal probabilities for a generator to be chosen
    weights <- rep(1, length(generator_quos))
  }
  assert_is_numeric(weights)

  # check integrity of start and end-column
  assert_is_integer(ind_tbl$start)
  assert_is_integer(ind_tbl$end)

  # add signal-column (list column of tibbles)
  dplyr::mutate(ind_tbl, signal = purrr::map2(start, end, ~{
    time <- .x:.y # time column in resulting signal-tibble
    subsig_len <- length(time) # length of the subsignal
    mask$n <- subsig_len
    mask$x <- seq_along(time)
    mask$c_x <- mask$x - round(subsig_len / 2, 0) # center x around 0

    # choose random generator-quosure
    generator_quo <- generator_quos[[sample(choices_gen, 1, prob = weights)]]

    # generate signal-values
    sig_val <- rlang::eval_tidy(generator_quo, data = mask)
    assert_is_numeric(sig_val)
    assert_equal_lengths(sig_val, time)

    # add variable amount of noise to signal-values
    noise_amount <- sample(choices_noise_amount, 1)
    sig_val <- sig_val + noise_amount * noise_fun(subsig_len, sd = sd(sig_val))

    # make sure no elements are near 0 (add 1e-4 to it); gets done to ensure,
    # that a subsignal doesen't get split if elements close to 0 are contained
    # (e.g. when processed by mbte_extract_subsignals())
    repeat({
      near_0 <- dplyr::near(sig_val, 0, tol = 1e-5)
      if (!any(near_0)) {
        break
      }

      sig_val[near_0] <- sig_val[near_0] + 1e-4
    })

    # return signal-tibble
    tibble::tibble(time = time, value = sig_val)
  }))
}

# a wrapper around gen_sim_sig() with predefined trends (linear, logarithmic,
# sigmoid, 2 x noisy signal) - experimental
gen_sim_sig_default <- function(ind_tbl, ...) {
  gen_sim_sig(ind_tbl, ...,
    r_coef(0.1, 2) * x,
    r_coef(0.1, 3) * log(1 + x),
    # sigmoid
    rs_coef(4) * (1 / (1 + exp(-(rs_coef(1) * c_x)))),
    rs_coef(4) * rnorm(n), # relatively strong noise
    rs_coef(1) * rnorm(n), # signal less strong but noisy
    weights = c(1, 2, 2, 3, 4)
  )
}

# generate actual raw dataset - take input of gen_sim_sig() or
# gen_sim_sig_default() as first input
#
# `long` specifies if the resulting dataset should be in long form (if `TRUE`)
# or wide (if `FALSE`)
#
# `mv_prefix` is the prefix used for a simulated measurement variable (assuming
# the resulting dataset should miminc the data obtained from measurements).
# Later on, this can be thought of the grouping-variable.
gen_sim_raw <- function(sig_tbl, long = TRUE, mv_prefix = "mv") {
  stopifnot(
    tibble::is_tibble(sig_tbl),
    rlang::is_scalar_logical(long),
    rlang::is_scalar_character(mv_prefix) && nzchar(mv_prefix) != 0
  )

  # assert integrity of needed columns
  assert_is_integer(sig_tbl$col)
  assert_is_list(sig_tbl$signal)

  # the correct numerical order (to avoid alphabetical column-ordering like
  # "mv1", "mv10", "mv2")
  correct_col_order <- paste0(mv_prefix, sort(sig_tbl$col))

  # NOTE: the following dplyr::spread()-call is needed in any case, even if
  # tidyr::gather() may be called later (if long is `TRUE`). the spread()-call
  # should add the desired 0-padding
  raw_tbl <- sig_tbl %>%
    tidyr::unnest(signal) %>%
    # only keep relevant variables
    dplyr::select(time, col, value) %>%
    # add prefix to column number
    dplyr::mutate(col = paste0(mv_prefix, col)) %>%
    tidyr::spread(col, value, fill = 0) %>%
    # put column in the right order (e.g. | time | mv1 | mv2 | ... | mv10 |)
    dplyr::select(time, !!!correct_col_order)

  if (long) {
    tidyr::gather(raw_tbl, !!mv_prefix, "value", !!!correct_col_order)
  } else {
    raw_tbl
  }
}

# generate a simulated raw dataset using (gen_sim_int(), gen_sim_sig_default()
# and gen_sim_raw()) - by default a cached version is used (computed using
# the defaults and the global testing seed)
gen_raw_dataset <- local({
  cached_dataset <- NULL

  # actual function to generate the dataset
  gen_dataset <- function(long, mv_prefix) {
    gen_sim_ind(nrow = 100L, ncol = 42L, max_subsig = 3L) %>%
      gen_sim_sig_default() %>%
      gen_sim_raw(long = long, mv_prefix = mv_prefix)
  }

  function(long = TRUE, mv_prefix = "mv", cached = TRUE) {
    if (!cached || !long || mv_prefix != "mv") {
      # recompute the dataset without wrapping it in withr::with_seed() - assume
      # the user takes care of reproducible random number generation
      gen_dataset(long, mv_prefix)
    } else {
      if (is.null(cached_dataset)) {
        # compute the "cached" version
        withr::with_seed(testing_seed(), {
          cached_dataset <<- gen_dataset(long = TRUE, mv_prefix = "mv")
        })
      }

      cached_dataset
    }
  }
})

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

# generate a random string consisting of lowercase letters of specified length
gen_random_string <- function(length = 6) {
  paste(sample(letters, length, TRUE), collapse = "")
}

# generate a random symbol of the specified length (all lowercase letters used)
gen_random_sym <- function(length = 6) {
  rlang::sym(gen_random_string(length))
}
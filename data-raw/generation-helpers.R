# helpers for dataset-generation

# load needed packages
library(tibble)
library(rlang)
library(purrr)
library(dplyr)
library(tidyr)

# Generate an index table
#
# Parameters:
#
# + nrow: The length of the signals to generate (number of rows the final
#   signal-table should have).
# + param: ncol Number of signals to generate.
# + max_subsig: The maximum number of subsignals the generate signals may
#   contain (from 1 to `max_subsig`).
#
# Return: A tibble with the following structure (all integer columns):
#
# + col: The number of the generated signal.
# + signal_nr: The number of the subsignal.
# + start: The starting indices of the corresponding subsignal.
# + end: The ending indices of the corresponding subsignal.
#
# NOTE: every row is a subsignal
gen_sim_ind <- function(nrow, ncol, max_subsig) {
  stopifnot(is.integer(nrow), is.integer(ncol), is.integer(max_subsig))

  # to choose number of subsignals in signal randomly
  choices_n_subsig <- seq_len(max_subsig)
  # subsignal positions (both start and end) to sample from
  choices_subsig_ind <- seq_len(nrow)

  map_dfr(seq_len(ncol - 1), ~{
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

    is_start_ind <- as.logical((seq_along(indices) %% 2) == 1)

    tibble(
      col = .x, signal_nr = seq_len(n_subsig), start = indices[is_start_ind],
      end = indices[!is_start_ind]
    )
  }) %>%
    # ensure the range of [1, nrow] (otherwise it's not guaranteed that 1 is
    # the minimum of starting indices and `nrow` likewise not the maximum)
    add_row(col = ncol, signal_nr = 1L, start = 1L, end = nrow)
}

# Generate simulated signals
#
# Use the output of gen_sim_ind() and add the signal-column (list column with
# `time` and `value`-columns).
#
# Parameters:
# + ind_tbl: The output of a call to gen_sim_ind() (index-table).
# + `...`: The elements of the ellipsis are used as quosures, which generate
#     the values of the subsignals (=> generators-quosures).
# + noise_fun: A function (rnorm by default), which is used to add noise to the
#   generated signals. It gets invoked with the first element being the number
#   of random values to generate. Additionally, `sd` gets passed (standard
#   deviation of the generated subsignal).
# + weights: A numeric vector for sampling probabilities (for the
#   generator-quosures).
#
# The following symbols are masked (for generator-quosures):
#
# + n: The length of the subsignal to generate.
# + x: Equivalent to `seq_len(n)` - can be seen as a simulated "time".
# + c_x: `x` centered around `0`.
# + coefficient functions: r_coef() and rs_coef() - are used to generate
#   randomized coefficients from a specified interval. These functions are
#   defined below.
#
# Return: The signal-column gets added to the index-table.
gen_sim_sig <- function(ind_tbl, ..., noise_fun = rnorm, weights = NULL) {
  stopifnot(is_tibble(ind_tbl))

  generator_quos <- enquos(...)
  stopifnot(length(generator_quos) != 0)

  # to choose a random generator
  choices_gen <- seq_along(generator_quos)

  # amount of noise to add (multiplied by the output of `noise_fun`)
  choices_noise_amount <- seq(0.1, 0.8, 0.1)

  # masking environment for tidy evalutation
  mask <- new_environment(coef_funs)

  if (missing(weights)) {
    # assume the user wanted equal probabilities for a generator to be chosen
    weights <- rep(1, length(generator_quos))
  }
  stopifnot(is.numeric(weights), length(weights) == length(generator_quos))

  # check integrity of start and end-column
  stopifnot(is.integer(ind_tbl$start))
  stopifnot(is.integer(ind_tbl$end))

  # add signal-column (list column of tibbles)
  mutate(ind_tbl, signal = map2(start, end, ~{
    time <- .x:.y # time column in resulting signal-tibble
    subsig_len <- length(time) # length of the subsignal
    mask$n <- subsig_len
    mask$x <- seq_along(time)
    mask$c_x <- mask$x - round(subsig_len / 2, 0) # center x around 0

    # choose random generator-quosure
    generator_quo <- generator_quos[[sample(choices_gen, 1, prob = weights)]]

    # generate signal-values
    sig_val <- eval_tidy(generator_quo, data = mask)
    stopifnot(is.numeric(sig_val))
    stopifnot(length(sig_val) == length(time))

    # add variable amount of noise to signal-values
    noise_amount <- sample(choices_noise_amount, 1)
    sig_val <- sig_val + noise_amount * noise_fun(subsig_len, sd = sd(sig_val))

    # make sure no elements are near 0 (add 1e-4 to it); gets done to ensure,
    # that a subsignal doesen't get split if elements too close to 0 are
    # contained (e.g. when processed by mbte_extract_subsignals())
    repeat({
      near_0 <- near(sig_val, 0, tol = 1e-5)
      if (!any(near_0)) {
        break
      }

      sig_val[near_0] <- sig_val[near_0] + 1e-4
    })

    # return signal-tibble
    tibble(time = time, value = sig_val)
  }))
}

# A wrapper around gen_sim_sig() with predefined trends (linear, logarithmic,
# sigmoid, 2 x noisy signal)
gen_sim_sig_default <- function(ind_tbl, ...) {
  gen_sim_sig(
    ind_tbl, ...,
    r_coef(0.1, 2) * x,
    r_coef(0.1, 3) * log(1 + x),
    rs_coef(4) * (1 / (1 + exp(-(rs_coef(1) * c_x)))), # sigmoid
    rs_coef(4) * rnorm(n), # noise with high intensity
    rs_coef(1) * rnorm(n), # noise with lower signal-intensity
    weights = c(1, 2, 2, 3, 4)
  )
}

# Generate actual raw dataset
#
# Use input from gen_sim_sig() or gen_sim_sig_default() as input.
#
# Parameters:
#
# + sig_tbl: The output of gen_sim_sig().
# + long: A logical indicating if the resulting table should be returned
#   in long- (if set to `TRUE`) or wide format (if set to `FALSE`).
# + mv_prefix: The prefix used for a simulated measurement variable/parameter
#   (assuming the resulting dataset should miminc the data obtained from
#   measurements).
gen_sim_raw <- function(sig_tbl, long = TRUE, mv_prefix = "mv") {
  stopifnot(
    is_tibble(sig_tbl),
    is_scalar_logical(long),
    is_scalar_character(mv_prefix) && nzchar(mv_prefix) != 0
  )

  # assert integrity of needed columns
  stopifnot(is.integer(sig_tbl$col))
  stopifnot(is.list(sig_tbl$signal))

  # the correct numerical order (to avoid lexical column-ordering like
  # "mv1", "mv10", "mv2")
  correct_col_order <- paste0(mv_prefix, sort(sig_tbl$col))

  # NOTE: the following dplyr::spread()-call is needed in any case, even if
  # tidyr::gather() may be called later (if long is `TRUE`). The spread()-call
  # adds the desired 0-padding to subsignals.
  raw_tbl <- sig_tbl %>%
    unnest(signal) %>%
    # only keep relevant variables
    select(time, col, value) %>%
    # add prefix to column number
    mutate(col = paste0(mv_prefix, col)) %>%
    spread(col, value, fill = 0) %>%
    # put column in the right order (e.g. | time | mv1 | mv2 | ... | mv10 |)
    select(time, !!!correct_col_order)

  if (long) {
    gather(raw_tbl, !!mv_prefix, "value", !!!correct_col_order)
  } else {
    raw_tbl
  }
}

#### coefficient-generation functions ####

# Helper-functions for gen_sim_sig() to generate randomized coefficients.

# Generate a random coefficient withing the range [min, max].
# NOTE: the coefficient gets sampled from the speciefied interval with 50
# subdivisions
r_coef <- function(min, max) {
  sample(seq(min, max, length.out = 50L), 1L)
}

# Similar to r_coef(), but sample from the symmetric interval
# [-value, value].
rs_coef <- function(value) {
  abs_value <- abs(value)
  r_coef(-abs_value, abs_value)
}

coef_funs <- list(
  r_coef = r_coef,
  rs_coef = rs_coef
)
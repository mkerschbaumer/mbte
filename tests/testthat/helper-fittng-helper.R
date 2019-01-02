# generate reference datasets (with fits) for testing correctness of output

ref_linear <- mbte_fit(filtered_signals, lin = lm(value ~ t, .signal))

ref_exponential <- mbte_fit(filtered_signals,
  exp = {
    # use log transformation like fitting helper is supposed to
    .signal$value <- log(.signal$value)
    model <- lm(value ~ t, .signal)
    predictions <- predict(model, .signal)
    exp(predictions)
  }
)

# a dataset for fitting - needed to test exponential fitting helper on a
# dataset with negative values
dat_neg_exponential <- local({
  # provide dummy grouping variable for mbte_nest_signals()
  raw_dataset <- tibble(time = 1:10, value = -1:8, group = 1)
  tbl_mbte <- raw_dataset %>%
    new_tbl_mbte(time, value) %>%
    mbte_nest_signals(group)
})

# generate a dataset containing 1 logistic signal with the passed parameters
#
# NOTE: noise is added
gen_log_testdataset <- function(A, B, C, D) {
  # create a logistic signal with noise (ensure reproduciblity)
  raw_dataset <- withr::with_seed(42L, tibble(
    time = 1:30,
    value = A / (1 + exp(B * (C - time))) + D + rnorm(30, sd = 0.2),
    group = 1 # dummy grouping variable
  ))
  raw_dataset %>%
    new_tbl_mbte(time, value) %>%
    mbte_nest_signals(group)
}

# parameters to test
logistic_testcases <- tibble::tribble(
  ~A, ~B, ~C, ~D,
  3, 0.4, 20, 3,
  2, 3.1, 5, -1,
  -4, 0.3, 10, 5
)

# create datasets for sigmoid fitting (positive test - no errors expected)
dat_logistic <- purrr::pmap(logistic_testcases, gen_log_testdataset)

# generate dataset of expected fitted values by providing the exact starting
# values to nls2()
gen_log_reference <- function(dat_logistic, A, B, C, D) {
  res <- dat_logistic
  fit <- nls2::nls2(
    value ~ A / (1 + exp(B * (C - time))) + D,
    res$signal[[1]],
    start = list(A = A, B = B, C = C, D = D),
    algorithm = "port",
    control = nls.control(maxiter = 500)
  )
  res$fits <- list(tibble(log = predict(fit)))
  res
}

# reference datasets for logistic fitting (positive test)
ref_logistic <- purrr::pmap(
  dplyr::bind_cols(
    tibble(dat_logistic = dat_logistic),
    logistic_testcases
  ),
  gen_log_reference
)

# dataset to provoce a convergence failure (nls2() should not be able to fit a
# logistic signal to the dataset)
dat_convergence_failure <- local({
  raw_dataset <- tibble(
    time = 1:30,
    value = sin(time),
    group = 1 # dummy grouping variable
  )
  raw_dataset %>%
    new_tbl_mbte(time, value) %>%
    mbte_nest_signals(group)
})

# should be the first helper file to be loaded

testing_seed <- function() {
  seed <- getOption("mbte.testseed", 42L)
  stopifnot(purrr::is_scalar_integer(seed))

  seed
}
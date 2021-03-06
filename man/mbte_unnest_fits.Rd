% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unnest.R
\name{mbte_unnest_fits}
\alias{mbte_unnest_fits}
\title{Perform unnesting of fitting data}
\usage{
mbte_unnest_fits(x)
}
\arguments{
\item{x}{A \code{\link{tbl_mbte}}.}
}
\value{
A \code{\link{tbl_mbte}} with the following columns:
\describe{
  \item{desc}{`desc` is a placeholder for all columns except the `signal`-
    and the `fits`-columns. Those columns can be seen as "descriptive"
    columns describing the measured signals.}
  \item{fit}{The name of the method used for fitting (character). See
    \code{\link{mbte_fit}} for details.}
  \item{time}{The time-column unnested from the `signal`-column.}
  \item{value}{The values of the predicted signals (originally in `signal`-
    column).}
}
}
\description{
The `fits`-list column gets unnested. This function is useful if the fitted
signal-values are processed further.
}
\examples{
# load sample dataset
data(filtered_signals)
filtered_signals

# perform fitting
fitted <- mbte_fit(filtered_signals, lm = lm(value ~ t, .signal))

# unnest predictions for signal-values (==> fitted signals)
#
# NOTE: "lm" is the only method used for fitting and therefore the `fit`-
# column consists only of "lm"
mbte_unnest_fits(fitted)

}
\seealso{
\code{\link{filtered_signals}} (dataset used in examples)

Other unnesting functions: \code{\link{mbte_unnest_signals}}
}
\concept{unnesting functions}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unnest.R
\name{mbte_unnest_signals}
\alias{mbte_unnest_signals}
\title{Perform unnesting of signals}
\usage{
mbte_unnest_signals(x)
}
\arguments{
\item{x}{A \code{\link{tbl_mbte}}.}
}
\value{
A \code{\link{tbl_mbte}} with the following columns:
\describe{
  \item{desc}{`desc` is a placeholder for all the columns except the `fits`-
    column. Those columns can be seen as descriptive columns.}
  \item{time}{The time column (originally in the `signal`-column).}
  \item{value}{The signal-values (before in the `signal`-list-column).}
}
}
\description{
The `signal`-list column gets unnested. This function can be seen as the
inversion of \code{\link{mbte_nest_signals}}.
}
\note{
The `fits`-column will be removed, if it is present.
}
\examples{
library(dplyr, warn.conflicts = FALSE)
data(raw_signals)
raw_signals

# create a tbl_mbte to nest
tbl <- raw_signals \%>\%
  group_by(mv) \%>\%
  new_tbl_mbte(time = "t", value = "value")

# nest signals and unnest them (should be equal to `tbl`)
nested <- mbte_nest_signals(tbl)
unnested <- mbte_unnest_signals(nested)

all.equal(unnested, tbl) # TRUE
\dontshow{stopifnot(isTRUE(all.equal(unnested, tbl)))}

}
\seealso{
\code{\link{raw_signals}} (dataset used in examples)

Other unnesting functions: \code{\link{mbte_unnest_fits}}
}
\concept{unnesting functions}

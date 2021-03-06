% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{filtered_signals}
\alias{filtered_signals}
\title{Filtered raw-signals}
\description{
A \code{\link{tbl_mbte}} based on the \code{\link{raw_signals}}-dataset.
Signal-extraction has already been performed and short signals were removed.
}
\details{
The following operations have been performed on \code{\link{raw_signals}}:
\itemize{
  \item conversion to a \code{\link{tbl_mbte}} via \code{\link{new_tbl_mbte}}
  \item nesting of `time`- and `value`-column via
    \code{\link{mbte_nest_signals}} (create `signal` list-column consisting
    of tibbles containing the time- and value-columns).
  \item extraction of subsignals via \code{\link{mbte_extract_subsignals}}
  \item filtering based on subsignal-length (only keep subsignals with a
    length greater than 20).
}
}
\section{Contained columns}{

\describe{
  \item{mv}{The measurement variable. See \code{\link{raw_signals}} for
    details.}
  \item{signal_nr}{An integer column indicating the number of the subsignal
    within the original signal. See \code{\link{mbte_extract_subsignals}} for
    details.}
  \item{signal}{A list-column consisting of tibbles.}
}
}

\seealso{
Other datasets: \code{\link{raw_signals}}
}
\concept{datasets}
\keyword{data}

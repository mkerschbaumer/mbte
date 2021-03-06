% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_mbte.R
\name{tbl_mbte}
\alias{tbl_mbte}
\title{The main data-structure of the mbte-package}
\description{
A \emph{tbl_mbte} is a \code{\link[tibble]{tibble}} under the hood.
Additionally, attributes are stored, which contain column-names. All
mentioned column names are stored as \code{\link[base:name]{symbols}}.
}
\section{Columns}{

The following columns are used during the analysis (the attributes storing
the column names have the same names).
\describe{
  \item{time}{A numeric column containing the time of a specific
    measurement.}
  \item{value}{A numeric column containing the measured value of a signal
    (e.g. intensity).}
  \item{signal}{The time- and value-column combined into a list-column
    (e.g. by using \code{\link{mbte_nest_signals}}). Every element of this
    column is a \code{\link[tibble]{tibble}} containing the time- and
    value-column of the signal.}
  \item{fits}{This list-column also contains tibbles. The fitted
    signal-values are stored as columns. NOTE: Every contained tibble has the
    same number of rows as the signal-tibble of the same row. This column may
    be generated using \code{\link{mbte_fit}}.}
}
}

\seealso{
Other tbl_mbte functions: \code{\link{is_tbl_mbte}},
  \code{\link{mbte_reconstruct}},
  \code{\link{new_tbl_mbte}}
}
\concept{tbl_mbte functions}

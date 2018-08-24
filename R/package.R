#' Model-based trend estimation
#'
#' This package is inspired by the
#' \href{https://speakerdeck.com/hadley/managing-many-models}{Managing many
#' models}-talk by Hadley Wickham. The intuition is to fit arbitrary models
#' to a signal. Those models are used to generate predictions. An (error)-metric
#' gets computed, based on the predicted and the original value of the signal.
#' Using \href{https://dplyr.tidyverse.org/}{dplyr}, relevant
#' measurement-variables can be kept and investigated further by filtering the
#' metric-results. The results can be visualized using
#' \href{https://ggplot2.tidyverse.org/}{ggplot2}.
#'
#' The general workflow and the way the different components of this package
#' interact, can be seen in the introduction vignette.
#'
#' @docType package
#' @name mbte-package
#' @aliases mbte
NULL
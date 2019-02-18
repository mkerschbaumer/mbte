# plotting functions of the mbte-package

#' Create a basic plot of signals or fits
#'
#' A \code{\link{tbl_mbte}} is passed. It is assumed, that the table is ordered
#' (panels to draw first are at the top of the table). The contents of the
#' panels (subplots) to draw are determined by the grouping specified in the
#' ellipsis (\code{...}). Since the number of panels to draw may not fit onto
#' one plot (can only contain \code{nrow * ncol} elements), panels are split
#' onto mulitple "pages". This is done by returning a list, where each element
#' is a ggplot2-chart, showing a subset of the data to plot.
#'
#' The user should pass a \code{\link[rlang:quotation]{quosure}} as \code{expr},
#' which specifies what should be drawn in a panel/subplot. It can make use of
#' the following masked objects:
#'
#' \describe{
#'   \item{.u_signals}{Unnested signals (see \code{\link{mbte_unnest_signals}})}
#'   \item{.u_fits}{Unnested fits (see \code{\link{mbte_unnest_fits}})}
#'   \item{.time_sym}{Time variable as a \code{\link[base:name]{symbol}}}
#'   \item{.value_sym}{Value-variable as a \code{\link[base:name]{symbol}}}
#'   \item{.n_pages}{The total number of pages, that will be printed}
#'   \item{.n_page}{The number of the current page (when evaluating
#'     \code{expr})}
#' }
#'
#' @note \code{.u_signals} and \code{.u_fits} are only available if the `signal`
#' and `fits`-columns are present.
#'
#' @param x A \code{\link{tbl_mbte}}.
#' @param expr An expression to generate one panel of the plot (gets quoted)
#' @param ... Variables used for grouping (get quoted).
#' @param nrow Number of rows per page.
#' @param ncol Number of columns per page.
#'
#' @return
#' A list of ggplot-objects.
#'
#' @seealso \code{\link{filtered_signals}} (dataset used in examples),
#' \code{\link[ggplot2]{facet_wrap}}
#'
#' @examples
#' library(ggplot2)
#'
#' # load dataset
#' data(filtered_signals)
#' filtered_signals
#'
#' # Draw a graph grouped by the measurement variable (multiple subsignals will
#' # be displayed in one panel/subplot).
#' plots <- mbte_panel_plot(
#'   x = filtered_signals,
#'   expr = {
#'     # expression for the content a subplot/panel
#'     # NOTE: `.u_signals` is provided by masking
#'     ggplot(.u_signals, aes(t, value)) +
#'       geom_path(aes(group = signal_nr))
#'   },
#'   mv # grouping varibales
#' )
#'
#' # only show 2 "pages"
#' head(plots, n = 2)
#'
#' # Now grouping by measurement variable (measured parameter) and signal_nr is
#' # performed.
#' plots <- mbte_panel_plot(
#'   x = filtered_signals,
#'   expr = {
#'     # NOTE: Using a grouping aesthetic is not necessary, since only one line
#'     # per panel will be drawn (because a signal in the used dataset is
#'     # identified by the `mv` and the `signal_nr` column).
#'     ggplot(.u_signals, aes(t, value)) +
#'       geom_path()
#'   },
#'   mv, signal_nr # grouping variables
#' )
#'
#' # only display 2 elements of returned list
#' head(plots, n = 2)
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 facet_wrap is.ggplot
#' @importFrom purrr map
#' @importFrom rlang as_data_mask ensyms enquo eval_tidy
#' @export
mbte_panel_plot <- function(x, expr, ..., nrow = 3, ncol = 4) {
  assert_is_tbl_mbte(x)
  expr <- enquo(expr)

  # grouping variables
  gr_vars <- ensyms(...)
  x <- add_panel_id_column(x, gr_vars)

  # panel-labels (for facetting)
  unique_panel_id <- unique(x$.panel_id)
  assert_is_scalar_num(nrow)
  assert_is_scalar_num(ncol)
  # number of panels on one "page"
  n_panels <- nrow * ncol
  # number of elements of resulting list
  n_pages <- ceiling(length(unique_panel_id) / n_panels)

  # unnest signals and fits (if present)
  plotting_tables <- create_plotting_tables(x)

  # create data mask for tidy evaluation
  data_mask <- as_data_mask(
    list(
      .time_sym = colname_time(x),
      .value_sym = colname_value(x),
      .n_pages = n_pages
    )
  )

  # generate "pages"
  map(seq_len(n_pages), ~{
    # generate selection of panels to show
    min <- 1 + (.x - 1) * n_panels
    max <- .x * n_panels
    selected_panels <- unique_panel_id[min:max]

    # filter tables accordingly
    signals <- filter(plotting_tables$signals, .panel_id %in% !!selected_panels)
    fits <- filter(plotting_tables$fits, .panel_id %in% !!selected_panels)

    # add signals, fits and number of current page to data mask
    data_mask$.u_signals <- signals
    data_mask$.u_fits <- fits
    data_mask$.n_page <- .x

    # generate baseline plot using user-provided expression
    g <- eval_tidy(expr, data_mask)
    stopifnot(is.ggplot(g))

    # add facet-wrapping
    g +
      facet_wrap(~.panel_id, scales = "free", nrow = nrow, ncol = ncol)
  })
}

# add column `.panel_id` to `x`. A panelID will be a string, consisting of the
# values of the specified grouping variables, separated by '-'.
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom rlang expr eval_tidy
#' @importFrom purrr walk map_chr
add_panel_id_column <- function(x, gr_vars) {
  # make sure at least one grouping variables is provided
  if (length(gr_vars) == 0) {
    stop("No grouping variables for panel-creation specified!")
  }

  # Make sure all specified grouping columns are contained in dataset
  walk(gr_vars, ~{
    assert_has_column(x, .x, "grouping-column")
  })

  # expression to generate panel-ID's
  panel_id_expr <- expr(paste(!!!gr_vars, sep = "-"))

  x %>%
    mutate(.panel_id = map_chr(seq_len(nrow(x)), ~{
      row <- x[.x, ]
      eval_tidy(panel_id_expr, row)
    })) %>%
    # convert to factor to ensure the right order of the shown panels
    mutate(.panel_id = factor(.panel_id, levels = unique(.panel_id))) %>%
    mbte_reconstruct(x) # make sure a tbl_mbte gets returned
}

# used in dplyr::mutate()
globalVariables(".panel_id")

create_plotting_tables <- function(x) {
  # unnest signals if signal columns are present and return empty tibble
  # otherwise
  signals <- cond_unnest(x, colname_signal, mbte_unnest_signals)

  # unnest fits, if present, and use empty tibble otherwise
  fits <- cond_unnest(x, colname_fits, mbte_unnest_fits)

  list(
    signals = signals,
    fits = fits
  )
}

# perform conditional unnesting: if the colname-related function (like
# `colname_signal`) returns a column-name, which is present in `x`, unnesting
# will be performed using the unnesting function (`unnest_fun`). Otherwise an
# empty tibble will be returned.
#' @importFrom purrr as_mapper
#' @importFrom tibble tibble
cond_unnest <- function(x, colname_fun, unnest_fun) {
  colname_fun <- as_mapper(colname_fun)
  unnest_fun <- as_mapper(unnest_fun)

  if (as.character(colname_fun(x)) %in% colnames(x)) {
    unnest_fun(x)
  } else {
    tibble()
  }
}

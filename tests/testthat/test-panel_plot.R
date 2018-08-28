context("panel_plot")

# example 1
test_that("panel plot grouped by `mv`", {
  plots <- mbte_panel_plot(
    x = filtered_signals,
    expr = {
      ggplot2::ggplot(.u_signals, ggplot2::aes(t, value)) +
        ggplot2::geom_path(ggplot2::aes(group = signal_nr))
    },
    mv,
    ncol = 4,
    nrow = 4
  )

  # make sure only 2 "pages" have been produced
  expect_equal(length(plots), 2)

  # check output using vdiffr
  purrr::walk(1:2, ~{
    title <- paste0("panel_plot1_page", .x)
    fig <- plots[[.x]]
    vdiffr::expect_doppelganger(title, fig)
  })
})

# example 2
test_that("panel plot grouped by `mv` and `signal_nr`", {
  plots <- mbte_panel_plot(
    x = filtered_signals,
    expr = {
      ggplot2::ggplot(.u_signals, ggplot2::aes(t, value)) +
        ggplot2::geom_path()
    },
    mv, signal_nr,
    ncol = 4,
    nrow = 2
  )

  # 5 "pages" of output expected
  expect_equal(length(plots), 5)

  # compare elementwise
  purrr::walk(1:5, ~{
    title <- paste0("panel_plot2_page", .x)
    fig <- plots[[.x]]
    vdiffr::expect_doppelganger(title, fig)
  })
})

test_that("no grouping variable provided", {
  expect_error(
    mbte_panel_plot(
      x = filtered_signals,
      expr = {
        ggplot2::ggplot() # dummy
      }
      # no grouping variable
    ),
    regexp = "[nN]o.+group.+variable"
  )
})

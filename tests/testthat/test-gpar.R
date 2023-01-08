context("Test graphical parameters object")

test_that("pmx_gpar defaults are well setted", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_identical(
    gpars$draft,
    list(size = 5, label = "DRAFT", colour = "grey50", x = Inf, y = -Inf)
  )
  expect_identical(
    gpars$smooth,
    list(
      se = FALSE, linetype = 1, size = 1.5, method = "loess",
      colour = "red"
    )
  )
})

test_that("can test object is pmx_gpar", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_true(is.pmx_gpar(gpars))
})

test_that("can print pmx_gpar object", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_output(print(gpars), "\"hello\"")
})


test_that("pmx_gpar ranges are applied to the plot", {
  ctr <- theophylline()

  p <- pmx_plot_npde_pred(
    ctr=ctr,
    pmxgpar=pmx_gpar(
      ranges=list(x=c(0, 200), y=c(-2, 2))
    )
  )

  l <- ggplot2::ggplot_build(p)[["layout"]]
  f <- function(l) l[[1]][["range"]][["range"]]
  expect_gte(f(l[["panel_scales_x"]])[[1]], 0)
  expect_lte(f(l[["panel_scales_x"]])[[2]], 200)
  expect_gte(f(l[["panel_scales_y"]])[[1]], -2)
  expect_lte(f(l[["panel_scales_y"]])[[2]], 2)
})

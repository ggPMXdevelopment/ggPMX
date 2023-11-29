if (helper_skip()) {

  context("Test graphical parameters object")

  #------------------- pmx_gpar start--------------------------------------------
  test_that("pmx_gpar defaults are well setted", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    expect_identical(
      gpars$draft,
      list(size = 5, label = "DRAFT", colour = "grey50", x = Inf, y = -Inf)
    )
    expect_identical(
      gpars$smooth,
      list(
        se = FALSE, linetype = 1, linewidth = 1.5, method = "loess",
        colour = "red"
      )
    )
  })


  test_that("pmx_gpar params: labels result: identical inherits", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    expect_true(inherits(gpars, c("pmx_gpar", "list")))
  })


  test_that("pmx_gpar params: labels result: identical names", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    gpNames <- c(
      "axis.title", "axis.text", "ranges", "is.smooth",
      "smooth", "is.band", "band", "is.draft", "is.title",
      "draft", "discrete", "is.identity_line", "identity_line",
      "scale_x_log10", "scale_y_log10", "color.scales", "is.legend",
      "legend.position", "labels"
    )

    expect_setequal(names(gpars), gpNames)
  })

  #------------------- pmx_gpar end----------------------------------------------

  #------------------- is.pmx_gpar start-----------------------------------------

  test_that("can test object is pmx_gpar", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    expect_true(is.pmx_gpar(gpars))
  })


  test_that("is.pmx_gpar: numeric variable is not pmx gpar", {
    gpars <- 123
    expect_false(is.pmx_gpar(gpars))
  })


  test_that("is.pmx_gpar: NULL variable is not pmx gpar", {
    gpars <- NULL
    expect_false(is.pmx_gpar(gpars))
  })


  test_that("is.pmx_gpar: list is not pmx gpar", {
    gpars <- list("A", "B")
    expect_false(is.pmx_gpar(gpars))
  })


  test_that("is.pmx_gpar: params: NULL result: error x is missing", {
    expect_error(is.pmx_gpar())
  })


  test_that("is.pmx_gpar: params: x result: identical inherits", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    expect_true(inherits(is.pmx_gpar(gpars), "logical"))
  })

  #------------------- is.pmx_gpar end-------------------------------------------

  #------------------- print.pmx_gpar start--------------------------------------

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

  test_that("print.pmx_gpar: params: NULL result: error missing argument", {
    expect_error(print.pmx_gpar())
  })

  test_that("print.pmx_gpar params: x result: identical names", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    prNames <- c(
      "axis.title", "axis.text", "ranges", "is.smooth",
      "smooth", "is.band", "band", "is.draft", "is.title",
      "draft", "discrete", "is.identity_line", "identity_line",
      "scale_x_log10", "scale_y_log10", "color.scales", "is.legend",
      "legend.position", "labels"
    )
    expect_setequal(names(print.pmx_gpar(gpars)), prNames)
  })


  test_that("print.pmx_gpar params: x result: identical inherits", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    expect_true(inherits(print.pmx_gpar(gpars), c("pmx_gpar", "list")))
  })

  #------------------- print.pmx_gpar end-----------------------------------------

  #------------------- `[.pmx_gpar` start-----------------------------------------

  test_that("`[.pmx_gpar`: params: NULL result: error x is missing", {
    expect_error(`[.pmx_gpar`())
  })


  test_that("`[.pmx_gpar`: params: x, index result: identical names", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    sub_meth_gpar <- `[.pmx_gpar`(gpars, index = 1)
    subNames <- c(
      "axis.title", "axis.text", "ranges", "is.smooth",
      "smooth", "is.band", "band", "is.draft", "is.title",
      "draft", "discrete", "is.identity_line", "identity_line",
      "scale_x_log10", "scale_y_log10", "color.scales", "is.legend",
      "legend.position", "labels"
    )
    expect_setequal(names(sub_meth_gpar), subNames)
  })


  test_that("`[.pmx_gpar`: params: x, index  result: identical inherits", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    sub_meth_gpar <- `[.pmx_gpar`(gpars, index = 1)
    expect_true(inherits(sub_meth_gpar, "pmx_gpar"))
  })


  test_that("`[.pmx_gpar` params: x, index result: defaults are well setted", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    sub_meth_gpar <- `[.pmx_gpar`(gpars, index = 1)
    expect_identical(
      sub_meth_gpar$draft,
      list(size = 5)
    )
    expect_identical(
      sub_meth_gpar$smooth,
      list(
        se = FALSE
      )
    )
  })


  test_that("`[.pmx_gpar` params: x = NULL, index result: pmx_gpar identical inherits", {
    gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
    sub_meth_gpar <- `[.pmx_gpar`(x = NULL, index = 1)
    expect_true(inherits(sub_meth_gpar, c("pmx_gpar", "list")))
  })
  #------------------- `[.pmx_gpar` end-------------------------------------------
}

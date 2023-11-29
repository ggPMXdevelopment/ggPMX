if (helper_skip()) {
  library(ggplot2)

  context("Test plot_pmx.pmx_gpar")

  ctr <- theophylline()
  labels <- list("Concentration", "Volume", "Clearance")
  x <- "STUD"
  y <- "SEX"
  test_that("plot_pmx.pmx_gpar: params: NULL result: missing arguments", {
    expect_error(plot_pmx.pmx_gpar())
  })


  test_that("plot_pmx.pmx_gpar: params: gpar, p; result: error class gpar is not a pmx_gpar object", {
    p <- ctr %>% pmx_plot_individual(trans = "log10_y")
    expect_error(plot_pmx.pmx_gpar(gpar = TRUE, p))
  })


  test_that("plot_pmx.pmx_gpar: params: gpar, p; result: error class p is not a ggplot object", {
    gp <- pmx_gpar(
      labels = NULL,
      discrete = TRUE,
      is.smooth = FALSE
    )
    expect_error(plot_pmx.pmx_gpar(gpar = gp, p = "DIS"))
  })


  test_that("plot_pmx.pmx_gpar: params: gpar, p; result: error class smooth is not a list or NULL", {
    gp <- pmx_gpar(
      labels = NULL,
      discrete = TRUE,
      is.smooth = FALSE
    )
    p <- ctr %>% pmx_plot_individual(trans = "log10_y")
    gp$smooth <- 1
    expect_error(plot_pmx.pmx_gpar(gpar = gp, p))
  })


  test_that("plot_pmx.pmx_gpar: params: gpar, p; result: error class band is not a list or NULL", {
    gp <- pmx_gpar(
      labels = NULL,
      discrete = TRUE,
      is.smooth = FALSE
    )
    p <- ctr %>% pmx_plot_individual(trans = "log10_y")
    gp$band <- 1
    expect_error(plot_pmx.pmx_gpar(gpar = gp, p))
  })


  test_that("plot_pmx.pmx_gpar: params: gpar, p; result: error class
          labels are not a list or NULL", {
            gp <- pmx_gpar(
              labels = NULL,
              discrete = TRUE,
              is.smooth = FALSE
            )
            p <- ctr %>% pmx_plot_individual(trans = "log10_y")
            gp$labels <- 1
            expect_error(plot_pmx.pmx_gpar(gpar = gp, p))
          })
}

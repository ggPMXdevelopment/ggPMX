if (helper_skip()) {

  library(ggPMX)
  ctr <- theophylline()

  context("Test pmx_plot_eta_box function")

  #------------------- pmx_plot_eta_box start ------------------------------------------

  test_that("pmx_plot_eta_box: params: ctr, ...;  result: ggplot", {
    p <- ctr %>% pmx_plot_eta_box(shrink=list(fun="sd"))
    expect_true(is_ggplot(p))
  })

  test_that("pmx_plot_eta_box: params: ctr, ...;  result: error", {
    expect_error(ctr %>% pmx_plot_eta_box(shrink=list("sd")))
  })

  #------------------- pmx_plot_eta_box end ------------------------------------------
}

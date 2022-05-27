context("Test pmx_plot_eta_cats function")
ctr <- theophylline()
#------------------- pmx_plot_eta_cats start ------------------------------------------
test_that(
  "pmx_plot_eta_cats: params: strat.color; result: identical value",
  {
    p <- ctr %>% pmx_plot_eta_cats(is.strat.color = TRUE)
    expect_equal(p$plot_env$x$is.strat.color, TRUE)
  }
)
#------------------- pmx_plot_eta_cats end --------------------------------------------

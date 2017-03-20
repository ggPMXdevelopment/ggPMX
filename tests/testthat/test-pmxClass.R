context("Test pmxClass")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("can create pmx class", {
  ctr <- pmx_mlx("standing")
  expect_is(ctr, "pmxClass")
  expect_identical(
    ctr %>% plot_names(), 
    c("ipred_iwres", "time_npde", "pred_npde", "distri", "indiv")
  )
})

test_that("can print pmx class", {
  ctr <- pmx_mlx("standing")
  expect_output(print(ctr), "pmx object:")
})

test_that("can remove plot from pnmx class", {
  ctr <- pmx_mlx("standing")
  cplots <- ctr %>% plot_names()
  ctr$remove_plot(cplots[1])
  res <- setdiff(cplots, ctr %>% plot_names())
  expect_identical(res, cplots[1])
})

test_that("can get pmx class config", {
  ctr <- pmx_mlx("standing")
  cplots <- ctr %>% plot_names()
  conf <- ctr$get_config(cplots[1])
  clabels <- list(title = "IPRED versus IWRES", subtitle = "", 
                  x = "Individual prediction", 
                  y = "Individual weighted residuals")
  expect_identical(conf$gp$labels, clabels)
})

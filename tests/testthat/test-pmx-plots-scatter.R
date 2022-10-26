context("Test Scatter residual plots")

#------------------- pmx_plot_dv_pred start -----------------------------------
test_that("pmx_plot_dv_pred: params: controller result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_dv_pred(ctr = ctr), "gg"))
          })

test_that("pmx_plot_dv_pred: params: no result: error",
          {
            expect_error(pmx_plot_dv_pred())
          })

test_that("pmx_plot_dv_pred: params: not controller result: error",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_dv_pred(ctr = ctr))
          })

test_that("pmx_plot_dv_pred: scales='free' argument flows correctly", {
  p <- pmx_plot_dv_pred(
    theophylline(),
    strat.facet = ~SEX,
    facets=list(scales="free"),
    labels=list(title="")
  ) %>%
  ggplot_build

  # x and y ranges are equal per facet:
  expect_equal(
    p[["layout"]][["panel_scales_x"]][[1]][["range"]][["range"]],
    p[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]]
  )

  expect_equal(
    p[["layout"]][["panel_scales_x"]][[2]][["range"]][["range"]],
    p[["layout"]][["panel_scales_y"]][[2]][["range"]][["range"]]
  )

  # should be a narrower max y range for facet 1:
  expect_lt(
    p[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]][[2]],
    p[["layout"]][["panel_scales_y"]][[2]][["range"]][["range"]][[2]]
  )

  # check that "free" scales have been passed for both axes:
  expect_true(p[["layout"]][["facet_params"]][["free"]][["x"]])
  expect_true(p[["layout"]][["facet_params"]][["free"]][["y"]])
})

#------------------- pmx_plot_dv_pred end -------------------------------------

#------------------- pmx_plot_iwres_time start --------------------------------
test_that("pmx_plot_iwres_time: params: controller result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_iwres_time(ctr = ctr), "gg"))
          })

test_that("pmx_plot_iwres_time: params: no result: error",
          {
            expect_error(pmx_plot_iwres_time())
          })

test_that("pmx_plot_iwres_time: params: not controller result: error",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_iwres_time(ctr = ctr))
          })
#------------------- pmx_plot_iwres_time end ----------------------------------

#------------------- pmx_plot_npde_time start ---------------------------------
test_that("pmx_plot_npde_time: params: controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_npde_time(ctr = ctr), "gg"))
})

test_that("pmx_plot_npde_time: params: no result: error",
          {
            expect_error(pmx_plot_npde_time())
          })

test_that("pmx_plot_npde_time: params: not controller result: error",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_npde_time(ctr = ctr))
          })
#------------------- pmx_plot_npde_time end -----------------------------------

#------------------- pmx_plot_npde_pred start ---------------------------------
test_that("pmx_plot_npde_pred: params: controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_npde_pred(ctr = ctr), "gg"))
})

test_that("pmx_plot_npde_pred: params: no result: error",
          {
            expect_error(pmx_plot_npde_pred())
          })

test_that("pmx_plot_npde_pred: params: not controller result: error",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_npde_pred(ctr = ctr))
          })
#------------------- pmx_plot_npde_pred end -----------------------------------

#------------------- pmx_plot_abs_iwres_ipred start ---------------------------
test_that("pmx_plot_abs_iwres_ipred: params: controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_abs_iwres_ipred(ctr = ctr), "gg"))
})

test_that("pmx_plot_abs_iwres_time: params: controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_abs_iwres_time(ctr = ctr), "gg"))
})

test_that("pmx_plot_abs_iwres_ipred: params: no result: error",
          {
            expect_error(pmx_plot_abs_iwres_ipred())
          })

test_that(
  "pmx_plot_abs_iwres_ipred: params: not controller result: error",
  {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_abs_iwres_ipred(ctr = ctr))
  }
)
#------------------- pmx_plot_abs_iwres_ipred end -----------------------------

#------------------- pmx_plot_iwres_ipred start -------------------------------
test_that("pmx_plot_iwres_ipred: params: controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_iwres_ipred(ctr = ctr), "gg"))
})

test_that("pmx_plot_iwres_ipred: params: no result: error",
          {
            expect_error(pmx_plot_iwres_ipred())
          })

test_that(
  "pmx_plot_iwres_ipred: params: not controller result: error",
  {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_iwres_ipred(ctr = ctr))
  }
)
#------------------- pmx_plot_iwres_ipred end ---------------------------------

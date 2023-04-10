context("Test filtering of controller data")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("individual plot: get all pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("individual")
  expect_equal(length(p), 5)
})


test_that("individual plot: get single page", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("individual", 2)
  expect_true(inherits(p, "ggplot"))
})


test_that("smooth_with_bloq result: smoothing with/wo BLOQ data", {
  ctr <- theophylline(bloq=pmx_bloq(cens="AGE0", limit="WT0"))
  p1 <- pmx_plot_abs_iwres_ipred(ctr, smooth_with_bloq=FALSE)
  p2 <- pmx_plot_abs_iwres_ipred(ctr, smooth_with_bloq=TRUE)
  p3 <- pmx_plot_abs_iwres_ipred(ctr)
  p1_obj <- ggplot2::ggplot_build(p1)
  p2_obj <- ggplot2::ggplot_build(p2)
  p3_obj <- ggplot2::ggplot_build(p3)

  expect_false(identical(p1_obj[["data"]][[4]], p2_obj[["data"]][[4]]))
  expect_identical(p1_obj[["data"]][[4]], p3_obj[["data"]][[4]])
})

test_that("individual plot: get some pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("individual", c(2, 4))
  expect_equal(length(p), 2)
})


test_that("individual plot : don't exceed the effective number of pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("individual", 1:100)
  expect_equal(length(p), 5)
})


test_that("bloq data has separate colour", {
  ctr <- theophylline(bloq=pmx_bloq(cens="AGE0", limit="WT0"))

  plots_with_bloq <- c("individual", "abs_iwres_ipred", "abs_iwres_time",
    "iwres_ipred", "iwres_time", "npde_time", "npde_pred", "dv_pred", "dv_ipred")

  lapply(
    paste0("pmx_plot_", plots_with_bloq),
    function(x) {expect_equal("pink", get(x)(ctr)[["plot_env"]][["bloq"]][["colour"]])}
  )
})


test_that("can create a plot using setting dname", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  ctr %>% set_plot("DIS", pname = "distr1", type = "box", dname = "eta")
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  expect_equal(length(pconf$plot$layers), 5)
})


test_that("controller bloq parameters are applied to the plots", {
  ctr_no_bloq <- theophylline(bloq=pmx_bloq(cens="AGE0", limit="WT0", show=FALSE))
  ctr_show_bloq <- theophylline(bloq=pmx_bloq(cens="AGE0", limit="WT0", show=TRUE))

  plots_with_bloq <- c("individual", "abs_iwres_ipred", "abs_iwres_time",
    "iwres_ipred", "iwres_time", "npde_time", "npde_pred", "dv_pred", "dv_ipred")

  getFunBloq <- function(s, ctr) {
      get(paste0("pmx_plot_", s))(ctr)[["plot_env"]][["bloq"]]
  }
    
  lapply(plots_with_bloq, function(s) {
      expect_null(getFunBloq(s, ctr_no_bloq))
      expect_false(is.null(getFunBloq(s, ctr_show_bloq)))
  })
})


test_that("Create a plot with not valid dname throw  message", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  expect_output(
    ctr %>% set_plot("DIS", pname = "distr1", type = "box", dname = "xxx"),
    "No data xxx provided for plot distr1"
  )
})

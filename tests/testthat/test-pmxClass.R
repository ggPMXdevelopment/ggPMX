context("Test pmxClass")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("can create pmx class", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  expect_identical(
    ctr %>% plot_names(), 
    c("ipred_iwres", "time_npde", "pred_npde", "distri", "indiv")
  )
})

test_that("can print pmx class", {
  ctr <- pmxClassHelpers$ctr
  expect_output(print(ctr), "pmx object:")
})

test_that("can remove plot from pnmx class", {
  ctr <- pmxClassHelpers$ctr
  cplots <- ctr %>% plot_names()
  ctr$remove_plot(cplots[1])
  res <- setdiff(cplots, ctr %>% plot_names())
  expect_identical(res, cplots[1])
})

test_that("can get pmx class config", {
  ctr <- pmxClassHelpers$ctr
  cplots <- ctr %>% plot_names()
  conf <- ctr$get_config(cplots[1])
  clabels <- list(title = "IPRED versus IWRES", subtitle = "", 
                  x = "Individual prediction", 
                  y = "Individual weighted residuals")
  expect_identical(conf$gp$labels, clabels)
})

test_that("can get data from controller", {
  ctr <- pmxClassHelpers$ctr
  peData <- ctr %>% get_data("estimates")
  peNames <- c("PARAM", "VALUE", "SE", "RSE", "PVALUE")
  expect_identical(names(peData), peNames)
  
  mpData <- ctr %>% get_data("predictions")
  mpNames <- c("ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES", "AMT", "DV",
               "EVID", "WT0", "AGE0", "SEX", "STUD")
  expect_identical(names(mpData), mpNames)
  
  ipData <- ctr %>% get_data("eta")
  ipNames <- c("ID", "EVID", "TWT", "TAGE", "SEX", "STUD", "VARIABLE", 
               "VALUE", "VAR", "EFFECT", "FUN")
  expect_identical(names(ipData), ipNames)
  
  fgData <- ctr %>% get_data("finegrid")
  fgNames <- c("ID", "TIME", "PRED", "IPRED")
  expect_identical(names(fgData), fgNames)
  
  sData <- ctr %>% get_data("shrink")
  sNames <- c("EFFECT", "SHRINK")
  expect_identical(names(sData), sNames)
})

test_that("can set plot and filter", {
  # set new plot
  ctr <- pmxClassHelpers$ctr
  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  expect_identical(dim(pconf$data[[2]]), c(150L, 10L))
  
  # set plot and filter
  ctr <- pmx_mlx("standing")
  ctr %>% set_plot("DIS", pname = "distr2", filter = ID < 10, type = "box")
  p <- ctr %>% get_plot("distr2")
  pconf <- ggplot2::ggplot_build(p)
  expect_identical(dim(pconf$data[[2]]), c(27L, 10L))
})


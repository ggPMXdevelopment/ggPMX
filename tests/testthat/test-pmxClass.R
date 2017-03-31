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

test_that("can get data from controller", {
  ctr <- pmx_mlx("standing")
  peData <- ctr %>% get_data("par_est")
  peNames <- c("PARAM", "VALUE", "SE", "RSE", "PVALUE")
  expect_identical(names(peData), peNames)
  
  mpData <- ctr %>% get_data("mod_pred")
  mpNames <- c("ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES", "AMT", "DV",
               "EVID", "WT0", "AGE0", "SEX", "STUD")
  expect_identical(names(mpData), mpNames)
  
  ipData <- ctr %>% get_data("ind_pred")
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

context("Test pmxClass")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("can create pmx class", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  expect_identical(
    sort(ctr %>% plot_names()),
    sort(c(
      "abs_iwres_ipred", "iwres_ipred", "npde_time", "iwres_time",
      "npde_pred", "dv_pred", "dv_ipred", "eta_hist", "eta_box", "individual",
      "eta_matrix", "eta_cats", "eta_conts",
      "iwres_qq", "npde_qq", "eta_qq", "iwres_dens"
    ))
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
  conf <- ctr$get_config("npde_time")
  clabels <- list(
    title = "NPDE vs TIME",
    subtitle = "",
    x = "TIME",
    y = "NPDE"
  )
  expect_identical(conf$gp$labels, clabels)
})

test_that("can get data from controller", {
  ctr <- pmxClassHelpers$ctr
  inputData <- ctr %>% get_data("input")
  inNames <- c("ID", "DV", "TIME", "SEX", "WT0", "AGE0", "STUD")
  expect_true(all(inNames %in% names(inputData)))

  peData <- ctr %>% get_data("estimates")
  peNames <- c("PARAM", "VALUE", "SE", "RSE", "PVALUE")
  expect_identical(names(peData), peNames)

  mpData <- ctr %>% get_data("predictions")
  mpNames <- c(
    "ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES", "DV",
    "SEX", "WT0", "AGE0", "STUD"
  )
  expect_true(all(mpNames %in% names(mpData)))

  fgData <- ctr %>% get_data("finegrid")
  fgNames <- c("ID", "TIME", "PRED", "IPRED")
  expect_true(all(fgNames %in% names(fgData)))
})

test_that("can set plot and filter", {
  # set new plot
  ctr <- pmxClassHelpers$ctr
  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  expect_equal(length(pconf$data), 4)
  # set plot and filter
  ctr %>% set_plot("DIS", pname = "distr2", filter = ID < 10, type = "box")
  p <- ctr %>% get_plot("distr2")
  pconf <- ggplot2::ggplot_build(p)
  expect_equal(length(pconf$data), 4)
})


test_that("can disable draft for all plots", {
  theophylline <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "theophylline"
  )
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")

  ctr <-
    pmx_mlx(
      config = "standing",
      directory = WORK_DIR,
      input = input_file,
      dv = "Y",
      dvid = "DVID",
      cats = c("SEX"),
      conts = c("WT0", "AGE0"),
      strats = "STUD",
      settings = pmx_settings(is.draft = FALSE)
    )

  is_draft <- vapply(
    ctr %>% plot_names(),
    function(p) {
      conf <- ctr %>% get_plot_config(p)
      conf$gp[["is.draft"]]
    }, TRUE
  )
  expect_false(any(is_draft))
})


test_that("can set draft to false for a single plot", {
  ctr <- pmxClassHelpers$ctr
  p <- ctr %>% pmx_plot_dv_pred(is.draft = FALSE)
})



test_that("can create a controller with data.frame as input", {
  theophylline <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "theophylline"
  )
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")

  dat <- read.csv(input_file)
  dat$SEX <- factor(dat$SEX, levels = c(0, 1), labels = c("M", "F"))


  ctr4 <- pmx(
    config = "standing", sys = "mlx",
    directory = WORK_DIR,
    input = dat,
    dv = "Y",
    dvid = "DVID",
    cats = "SEX"
  )

  expect_equal(nrow(ctr4 %>% get_data("input")), nrow(dat))
})

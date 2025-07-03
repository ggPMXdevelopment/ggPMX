if (helper_skip()) {

  library(ggPMX)
  library(ggplot2)
  library(purrr)
  ctr <- theophylline()

  context("Test pmx_plot_vpc function")

  #------------------- pmx_plot_vpc - start -------------------------------------

  test_that("pmx_plot_vpc: params: ctr, is.footnote; result: ggplot", {
    p <- pmx_plot_vpc(ctr, is.footnote = FALSE)
    expect_s3_class(p, "ggplot")
  })

  test_that("pmx_plot_vpc: params: ctr, strat.facet; result: ggplot", {
    p <- pmx_plot_vpc(ctr, strat.facet = ~STUD)
    expect_s3_class(p, "ggplot")
  })

  test_that("pmx_plot_vpc: params: ctr; result: ggplot", {
    p <- pmx_plot_vpc(ctr)
    expect_s3_class(p, "ggplot")
  })

  test_that("pmx_plot_vpc: params: ctr, bin; result: ggplot", {
    p <- pmx_plot_vpc(ctr, bin = pmx_vpc_bin(style = "equal"))
    expect_s3_class(p, "ggplot")
  })

  test_that("custom labels are applied to pmx_plot_vpc", {
    ctr <- theophylline()
    p <- pmx_plot_vpc(ctr, labels = c(x = "custom axis x", y = "custom axis y"))
    expect_identical(p[["labels"]][["x"]], "custom axis x")
    expect_identical(p[["labels"]][["y"]], "custom axis y")
  })

  test_that("pmx_plot_vpc: params NULL result: error missing arguments", {
    expect_error(pmx_plot_vpc())
  })


  test_that("pmx_plot_vpc: params ctr result: identical names", {
    p <- pmx_plot_vpc(ctr)
    expect_true(is_ggplot(p))
  })


  test_that("pmx_plot_vpc: params NULL result: identical type", {
    p <- ctr %>% pmx_plot_vpc()
    expect_true(is_ggplot(p))
    expect_identical(p$plot_env$type, "percentile")
    expect_identical(p$plot_env$idv, "TIME")
  })


  test_that("pmx_plot_vpc: params result: ggplot", {
    p <- ctr %>% pmx_plot_vpc(strat.facet = "SEX", facets = list(nrow = 2), type = "scatter")
    expect_true(is_ggplot(p))
  })


  test_that("pmx_plot_vpc: params result: identical type", {
    p <- ctr %>% pmx_plot_vpc(strat.facet = "SEX", facets = list(nrow = 2), type = "scatter")
    expect_identical(p$plot_env$type, "scatter")
  })


  test_that("pmx_plot_vpc: params result: ggplot, identical median", {
    vpc <- ctr %>% pmx_plot_vpc(
      is.legend = TRUE,
      pi = pmx_vpc_pi(interval = c(0.02, 0.98), median = list(linetype = "dotted")),
      ci = pmx_vpc_ci(interval = c(0.05, 0.95), median = list(fill = "red"))
    )
    expect_true(is_ggplot(vpc))
    expect_true(identical(vpc$plot_env$pi$median$linetype, "dotted"))
    expect_true(identical(vpc$plot_env$ci$median$fill, "red"))
  })
  
  test_that("Test different ways to facet vpc pmx_plot_vpc", {
    p1 <- ctr %>% pmx_plot_vpc(strat.facet = c("SEX", "STUD"))
    p2 <- ctr %>% pmx_plot_vpc(strat.facet = SEX~STUD)
    p3 <- ctr %>% pmx_plot_vpc(strat.facet = ~SEX+STUD)
    expect_identical(sort(as.character(p1$facet$params$facets)), sort(as.character(p2$facet$params$facets)))
    expect_identical(sort(as.character(p2$facet$params$facets)), sort(as.character(p3$facet$params$facets)))
  })

  #------------------- pmx_plot_vpc - end ---------------------------------------

  #------------------- pmx_vpc_pi - start ---------------------------------------

  context("Test pmx_vpc_pi function")

  test_that("pmx_vpc_pi: params result: 'pmx_vpc_pi', 'list'", {
    pi <- pmx_vpc_pi(interval = c(0.02, 0.98), median = list(linetype = "dotted"))
    expect_true(inherits(pi, c("pmx_vpc_pi", "list")))
  })


  test_that("pmx_vpc_pi: params NULL result: 'pmx_vpc_pi', 'list'", {
    pi <- pmx_vpc_pi()
    expect_true(inherits(pi, c("pmx_vpc_pi", "list")))
  })


  test_that("pmx_vpc_pi: params result: elements in the list", {
    piNames <- c("show", "probs", "median", "extreme", "area")
    expect_true(all(piNames %in% names(pmx_vpc_pi())))
  })


  test_that("pmx_vpc_pi: params result: identical structure", {
    pi <- pmx_vpc_pi()
    area_default <- list(fill = "blue", alpha = 0.1)
    expect_identical(pi$area, area_default)
  })

  #------------------- pmx_vpc_pi - end -----------------------------------------

  #------------------- pmx_vpc_obs - start --------------------------------------

  context("Test pmx_vpc_obs function")

  test_that("pmx_vpc_obs: params result: 'pmx_vpc_obs', 'list'", {
    obs <- pmx_vpc_obs(show = TRUE, color = "#000000", size = 1, alpha = 0.7, shape = 1)
    expect_true(inherits(obs, c("pmx_vpc_obs", "list")))
  })


  test_that("pmx_vpc_obs: params NULL result: 'pmx_vpc_obs', 'list'", {
    obs <- pmx_vpc_obs()
    expect_true(inherits(obs, c("pmx_vpc_obs", "list")))
  })


  test_that("pmx_vpc_obs: params result: elements in the list", {
    obsNames <- c("color", "size", "alpha", "shape")
    expect_true(all(obsNames %in% names(pmx_vpc_obs())))
  })


  test_that("pmx_vpc_obs: params result: NULL", {
    expect_true(is.null(names(pmx_vpc_obs(show = FALSE))))
  })

  #------------------- pmx_vpc_obs - end ----------------------------------------

  #------------------- pmx_vpc_ci - start ---------------------------------------

  context("Test pmx_vpc_ci function")

  test_that("pmx_vpc_ci: params result: 'pmx_vpc_ci', 'list'", {
    ci <- pmx_vpc_ci(interval = c(0.05, 0.95), median = list(fill = "red"))
    expect_true(inherits(ci, c("pmx_vpc_ci", "list")))
  })


  test_that("pmx_vpc_ci: params NULL result: 'pmx_vpc_ci', 'list'", {
    ci <- pmx_vpc_ci()
    expect_true(inherits(ci, c("pmx_vpc_ci", "list")))
  })


  test_that("pmx_vpc_ci: params NULL result: elements in the list", {
    ciNames <- c("show", "probs", "method", "median", "extreme")
    expect_true(all(ciNames %in% names(pmx_vpc_ci())))
  })


  test_that("pmx_vpc_ci: params result: elements in the list", {
    ci <- pmx_vpc_ci(interval = c(0.05, 0.95), median = list(fill = "red"))
    expect_identical(ci$probs, c(0.05, 0.95))
    expect_identical(ci$median$fill, "red")
  })

  test_that("pmx_vpc_ci: params result: error", {
    expect_error(pmx_vpc_ci(method = "triangle", median = list(fill = "red")))
  })

  #------------------- pmx_vpc_ci - end -----------------------------------------

  #------------------- pmx_vpc_rug - start --------------------------------------

  context("Test pmx_vpc_rug function")

  test_that("pmx_vpc_rug: params result: 'pmx_vpc_rug', 'list'", {
    obs <- pmx_vpc_rug(show = TRUE, color = "#000000", linewidth = 1, alpha = 0.7)
    expect_true(inherits(obs, c("pmx_vpc_rug", "list")))
  })


  test_that("pmx_vpc_rug: params NULL result: 'pmx_vpc_rug', 'list'", {
    obs <- pmx_vpc_rug()
    expect_true(inherits(obs, c("pmx_vpc_rug", "list")))
  })


  test_that("pmx_vpc_rug: params result: elements in the list", {
    obsNames <- c("color", "linewidth", "alpha")
    expect_true(all(obsNames %in% names(pmx_vpc_rug())))
  })


  test_that("pmx_vpc_rug: params result: NULL", {
    expect_true(is.null(names(pmx_vpc_rug(show = FALSE))))
  })

  #------------------- pmx_vpc_rug - end ----------------------------------------

  #------------------- quantile_dt - start --------------------------------------

  test_that("quantile_dt: params: NULL; result: missing arguments", {
    expect_error(quantile_dt())
  })

  #------------------- quantile_dt - end ----------------------------------------

  #------------------- pmx_vpc - start --------------------------------------

  test_that("pmx_vpc: params: NULL; result: identical inherits", {
    vpc <- pmx_vpc()
    expect_true(inherits(vpc, c("pmx_vpc", "pmx_gpar")))
    expect_true(inherits(vpc$pi, c("pmx_vpc_pi", "list")))
  })


  test_that("pmx_vpc: params: NULL; result: identical structure", {
    vpc <- pmx_vpc()
    expect_identical(vpc$ptype, "VPC")
    expect_true(vpc$strat)
  })

  test_that("pmx_vpc: params: type; result: identical structure (default type)", {
    vpc <- pmx_vpc(type = "percent")
    expect_identical(vpc$type, "percentile")
  })

  #------------------- pmx_vpc - end ----------------------------------------

  #------------------- vpc_footnote. - start --------------------------------------

  test_that("vpc_footnote.: params: x; result: identical inherits", {
    vpc <- pmx_vpc()
    vpc_f <- vpc_footnote.(vpc)
    expect_true(inherits(vpc_f, c("pmx_vpc", "pmx_gpar")))
    expect_true(inherits(vpc_f$ci, c("pmx_vpc_ci", "list")))
    expect_true(inherits(vpc_f$footnote, "character"))
  })

  test_that("vpc_footnote.: params: x; result: identical structure", {
    vpc <- pmx_vpc()
    vpc_f <- vpc_footnote.(vpc)
    expect_identical(vpc_f$gp$smooth$linetype, 1)
    expect_identical(vpc_f$gp$legend.position, "right")
  })

  test_that("vpc_footnote.: params: x; result: error", {
    vpc <- ""
    expect_error(vpc_footnote.(vpc))
  })

  #------------------- vpc_footnote. - end ---------------------------------------

  #------------------- vpc_legend. - start ---------------------------------------
  #
  test_that("vpc_legend.: params: x; result: identical structure", {
    vpc <- pmx_vpc(labels=list(title="x"))
    vpc_l <- vpc_legend.(vpc)
    expect_identical(vpc_l$ptype, "VPC")
    expect_identical(vpc_l$rug$alpha, 0.7)
  })

  test_that("vpc_legend.: params: x; result: identical inherits", {
    vpc <- pmx_vpc(labels=list(title="x"))
    vpc_l <- vpc_legend.(vpc)
    expect_true(inherits(vpc_l, c("pmx_vpc", "pmx_gpar")))
  })

  test_that("vpc_legend.: params: x; result: identical names", {
    vpc <- pmx_vpc(labels=list(title="x"))
    vpc_l <- vpc_legend.(vpc)
    vpslNames <- c(
      "ptype", "strat", "idv", "dname", "labels", "is.legend", "is.footnote",
      "type", "facets", "obs", "pi", "ci", "rug", "bin",
      "gp", "obs_legend", "sim_legend"
    )

    expect_identical(names(vpc_l), vpslNames)
  })
  #------------------- vpc_legend. - end -----------------------------------------

  #------------------- plot_pmx.pmx_vpc - start ----------------------------------

  test_that("plot_pmx.pmx_vpc: params: x; result: error missing arguments", {
    expect_error(plot_pmx.pmx_vpc())
  })

  #------------------- plot_pmx.pmx_vpc - end ------------------------------------
}

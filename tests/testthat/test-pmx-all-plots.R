if (helper_skip()) {

  context("Test all plots")
  pmxClassHelpers <- test_pmxClass_helpers()

  test_that("We can call all pmx_plot_xx with success", {
    ctr <- pmxClassHelpers$ctr
    pmx_plots <- ctr %>% plot_names()
    pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)

    res <- lapply(
      pmx_function_plots,
      function(fun) {
        is_function <- exists(fun, where = "package:ggPMX", mode = "function")
        if (is_function) {
          do.call(fun, list(ctr = ctr))
        } else {
          if (fun == "pmx_plot_indiv") {
            ctr %>% pmx_plot_individual(1)
          }
        }
      }
    )
    names(res) <- pmx_function_plots

    # Check ggmatrix
    expect_true(inherits(res[["pmx_plot_eta_matrix"]], "ggmatrix"))
    res$pmx_plot_eta_matrix <- NULL

    # Check that all plots are ggplot objects
    Map(
      names(res),
      res,
      f = function(name, plot) {
        expect_true(is_ggplot(plot))
      }
    )
  })

  test_that("We can call all pmx_plot_xx with title with success", {
    ctr <- theophylline(settings=pmx_settings(use.titles = TRUE))
    pmx_plots <- c(
      "abs_iwres_ipred", "iwres_ipred", "iwres_time", "iwres_dens", "vpc",
      "npde_time", "npde_pred", "dv_pred", "dv_ipred", "individual", "eta_hist",
      "eta_box", "eta_cats", "eta_conts"
    )

    pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)

    res <- lapply(
      pmx_function_plots,
      function(fun) {
        is_function <- exists(fun, where = "package:ggPMX", mode = "function")
        if (is_function) {
          do.call(fun, list(ctr = ctr, is.title = TRUE))
        }
      }
    )

    expect_true(all(vapply(res, function(x) x$labels$title != "", TRUE)))
    pmx_plots <- c("iwres_qq", "npde_qq", "eta_qq")
    pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)

    res <- lapply(
      pmx_function_plots,
      function(fun) {
        is_function <- exists(fun, where = "package:ggPMX", mode = "function")
        if (is_function) {
          do.call(fun, list(ctr = ctr, is.title = TRUE))
        }
      }
    )

    expect_true(all(vapply(res, function(x) x[["labels"]][["title"]] == "", TRUE)))
    p <- ctr %>% pmx_plot_eta_matrix(is.title = TRUE)
    expect_true(p[["title"]] != "")
  })

  test_that("We can call all pmx_plot_xx without title with success", {
    ctr <- theophylline()
    pmx_plots <- c(
      "abs_iwres_ipred", "iwres_ipred", "iwres_time", "iwres_dens",
      "iwres_qq", "npde_time", "npde_pred", "vpc", "npde_qq", "dv_pred",
      "dv_ipred", "individual", "eta_hist", "eta_box", "eta_cats", "eta_conts",
      "eta_qq", "saem_convergence"
    )

    pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)

    res <- lapply(
      pmx_function_plots,
      function(fun) {
        is_function <- exists(fun, where = "package:ggPMX", mode = "function")
        if (is_function) {
          do.call(fun, list(ctr = ctr, is.title = FALSE))
        }
      }
    )

    expect_true(all(vapply(res, function(x) x[["labels"]][["title"]] == "", TRUE)))
    p <- ctr %>% pmx_plot_eta_matrix(is.title = FALSE)
    expect_true(p[["title"]] == "")
  })


  context(" Test pmx_plot_generic function")

  #---------------------- pmx_plot_generic with nlmixr controller start ---------------------------------
  if (requireNamespace("nlmixr2est", quietly = TRUE)) {
    test_that("pmx_plot_generic with nlmixr controller: params: ctr, pname   result: identical inherits, names", {
      skip_on_os("windows")
      one.compartment <- function() {
        ini({
          tka <- 0.45 # Log Ka
          tcl <- 1 # Log Cl
          tv <- 3.45 # Log V
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          d / dt(depot) <- -ka * depot
          d / dt(center) <- ka * depot - cl / v * center
          cp <- center / v
          cp ~ add(add.sd)
        })
      }
      fit <- nlmixr2est::nlmixr(one.compartment, nlmixr2data::theo_sd, "saem",
                             control = list(print = 0)
                             )
      ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))
      p <- pmx_plot_generic(ctr, pname = "abs_iwres_ipred")

      expect_true(is.null(pmx_plot_generic(ctr, pname = "abs")))
      expect_true(is_ggplot(p))

    })

  }

  #---------------------- pmx_plot_generic with nlmixr controller end ---------------------------------

  #---------------------- pmx_plot_generic with theophylline contr. start ---------------------------------

  ctr <- theophylline()
  test_that("pmx_plot_generic: params: ctr, pname result: gg, gglpot", {
    p1 <- pmx_plot_generic(ctr, pname = "individual")
    p2 <- pmx_plot_generic(ctr, pname = "dv_ipred")
    p3 <- pmx_plot_generic(ctr, pname = "dv_pred")
    p4 <- pmx_plot_generic(ctr, pname = "abs_iwres_ipred")
    p5 <- pmx_plot_generic(ctr, pname = "iwres_dens")
    p6 <- pmx_plot_generic(ctr, pname = "npde_qq")
    p7 <- pmx_plot_generic(ctr, pname = "npde_pred")
    p8 <- pmx_plot_generic(ctr, pname = "npde_time")
    p9 <- pmx_plot_generic(ctr, pname = "eta_qq")
    p10 <- pmx_plot_generic(ctr, pname = "eta_matrix")
    p11 <- pmx_plot_generic(ctr, pname = "eta_box")
    p12 <- pmx_plot_generic(ctr, pname = "eta_hist")

    expect_true(inherits(p1, "list"))
    expect_true(is_ggplot(p2))
    expect_true(is_ggplot(p3))
    expect_true(is_ggplot(p4))
    expect_true(is_ggplot(p5))
    expect_true(is_ggplot(p6))
    expect_true(is_ggplot(p7))
    expect_true(is_ggplot(p8))
    expect_true(is_ggplot(p9))
    expect_true(inherits(p10, "ggmatrix"))
    expect_true(is_ggplot(p11))
    expect_true(is_ggplot(p12))

  })

  test_that("pmx_plot_generic: params: NULL result: error missing arguments", {
    expect_error(pmx_plot_generic())
  })

  test_that("pmx_plot_generic: params: ctr, pname result: NULL (p name is not
          in ctr %>% plot_names())", {
            expect_true(is.null(pmx_plot_generic(ctr, pname = "abs")))
          })

  test_that("pmx_plot_generic: params: ctr, pname result: identical names", {
    p <- pmx_plot_generic(ctr, pname = "abs_iwres_ipred")
    expect_true(is_ggplot(p))

  })


  test_that("pmx_plot_generic: params: ctr, pname result: identical structure", {
    p <- pmx_plot_generic(ctr, pname = "abs_iwres_ipred")
    expect_identical(p$plot_env$aess$x, "IPRED")
  })

  #---------------------- pmx_plot_generi cwith theophylline contr. end -----------------------------------

  test_that("pmx_plot_generic: params: ctr, pname  result: error class ctr is not pmxClass", {
    bloq <- pmx_bloq(cens = "BLOQ_name", limit = "LIMIT_name")
    expect_error(pmx_plot_generic(ctr = bloq, pname = "abs_iwres_ipred"))
  })

  #---------------------- wrap_pmx_plot_generic start ----------------------------

  context(" Test wrap_pmx_plot_generic function")
  test_that("wrap_pmx_plot_generic: params: NULL result: error missing arguments", {
    expect_error(wrap_pmx_plot_generic())
  })

  #---------------------- wrap_pmx_plot_generic end ------------------------------

  #---------------------- pmx_register_plot start --------------------------------

  test_that("pmx_register_plot: params: NULL result: error missing arguments", {
    expect_error(pmx_register_plot())
  })

  test_that("pmx_register_plot: params: ctr, pp, pname  result: identical inherits", {
    pp <- ctr %>% get_plot("individual")
    expect_true(is_ggplot(pmx_register_plot(ctr, pp[[1]], pname = "indiv1")))
  })

  test_that("pmx_register_plot: params: ctr, pname, pp  result: identical names", {
    pp <- ctr %>% get_plot("individual")
    p <- pmx_register_plot(ctr, pp[[1]], pname = "indiv1")
    expect_true(is_ggplot(p))
  })

  test_that("pmx_register_plot: params: ctr, pp  result: identical line color", {
    pp <- ctr %>% get_plot("individual")
    p <- pmx_register_plot(ctr, pp[[1]])
    expect_identical(p$plot_env$gp$identity_line$colour, "blue")
  })
  #---------------------- pmx_register_plot end ----------------------------------

  #---------------------- pmx_plot_cats start ------------------------------------

  test_that("pmx_plot_cats: params: NULL result: error missing arguments", {
    expect_error(pmx_plot_cats())
  })


  test_that("pmx_register_plot: params: ctr, pname  result: identical inherits", {
    p <- ctr %>% pmx_plot_cats("npde_time")
    expect_true(inherits(p, "list"))
  })


  test_that("pmx_register_plot: params: ctr, pname  result: identical inherits of the first ggplot", {
    p <- ctr %>% pmx_plot_cats("npde_time")
    expect_true(is_ggplot(p[[1]]))
  })


  test_that("pmx_register_plot: params: ctr, pname  result: identical ptype", {
    p <- ctr %>% pmx_plot_cats("npde_time")
    expect_identical(p[[1]]$plot_env$ptype, "SCATTER")
  })


  test_that("pmx_register_plot: params: ctr, pname  result: identical names", {
    p <- ctr %>% pmx_plot_cats("npde_time")
    expect_true(is_ggplot(p[[1]]))
  })

  test_that("pmx_register_plot: params: ctr, pname, cats  result: NULL", {
    p1 <- ctr %>% pmx_plot_cats("npde_time", cats = "")
    p2 <- ctr %>% pmx_plot_cats("npde_time", cats = NULL)
    expect_true(is.null(p1))
    expect_true(is.null(p2))
  })

  #---------------------- pmx_plot_cats end --------------------------------------
}

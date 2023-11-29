if (helper_skip() && requireNamespace("nlmixr2", quietly = TRUE)) {
  context("Test pmx_nlmixr controller")

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

  fit <- nlmixr2::nlmixr(one.compartment, nlmixr2data::theo_sd, "saem",
                         control = list(print = 0)
                         )

  #------------------- pmx_nlmixr - start ---------------------------------------
  test_that("pmx_nlmixr: params: result:  equal inherits", {
    ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))

    p1 <- ctr %>% pmx_plot_individual()
    p2 <- ctr %>% pmx_plot_dv_ipred()
    p3 <- ctr %>% pmx_plot_dv_pred()
    p4 <- ctr %>% pmx_plot_abs_iwres_ipred()
    p5 <- ctr %>% pmx_plot_individual(1)
    p6 <- ctr %>% pmx_plot_iwres_dens()
    p7 <- ctr %>% pmx_plot_npde_qq()
    p8 <- ctr %>% pmx_plot_npde_pred()
    p9 <- ctr %>% pmx_plot_npde_time()
    p10 <- ctr %>% pmx_plot_eta_qq()
    p11 <- ctr %>% pmx_plot_vpc()
    p12 <- ctr %>% pmx_plot_vpc(strat.facet = "SEX", facets = list(nrow = 2), type = "scatter")
    p13 <- ctr %>% pmx_plot_eta_box()
    p14 <- ctr %>% pmx_plot_eta_hist()
    p15 <- ctr %>% pmx_plot_eta_matrix()

    expect_true(inherits(p1, c("gg", "ggplot")))
    expect_true(inherits(p2, c("gg", "ggplot")))
    expect_true(inherits(p3, c("gg", "ggplot")))
    expect_true(inherits(p4, c("gg", "ggplot")))
    expect_true(inherits(p5, c("gg", "ggplot")))
    expect_true(inherits(p6, c("gg", "ggplot")))
    expect_true(inherits(p7, c("gg", "ggplot")))
    expect_true(inherits(p8, c("gg", "ggplot")))
    expect_true(inherits(p9, c("gg", "ggplot")))
    expect_true(inherits(p10, c("gg", "ggplot")))
    expect_true(inherits(p11, "NULL"))
    expect_true(inherits(p12, "NULL"))
    expect_true(inherits(p13, c("gg", "ggplot")))
    expect_true(inherits(p14, c("gg", "ggplot")))
    expect_true(inherits(p15, c("gg", "ggplot")))
  })


  test_that("pmx_nlmixr: params result: elements in the list", {
    configNames <- c("sys", "plots", "omega", "finegrid", "eta")
    ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))
    expect_true(all(configNames %in% names(ctr$config)))
  })

  test_that("pmx_nlmixr: params result: identical names", {
    pmxNames <- c(
      ".__enclos_env__", "sim_blq", "time", "id", "bloq", "sim", "plot_file_name", "report_n",
      "report_queue", "save_dir", "footnote", "warnings", "endpoint", "abbrev", "re", "has_re",
      "settings", "strats", "occ", "conts", "cats", "dvid", "dv", "input_file",
      "input", "config", "data", "clone", "post_load", "plots", "get_plot", "set_config",
      "get_config", "remove_plot", "update_plot", "add_plot", "dequeue_plot", "enqueue_plot", "print", "initialize"
    )
    ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))
    expect_equal(pmxNames, names(ctr))
  })

  test_that("pmx_nlmixr: params: fit, dvid, conts, cats, strats, ect.
            result:  identical value", {
              ep <- pmx_endpoint(
                code = "1",
                file.code = "1"
              )
              ctr <- pmx_nlmixr(fit, dvid = "TIME", conts = c("cl", "v"), endpoint = ep)
              expect_identical(ctr$endpoint$code, "1")
              expect_identical(ctr$dvid, "TIME")
            })

  test_that("pmx_nlmixr: params: fit, dvid, conts, cats, strats, ect.
            result:  identical value", {
              ep <- pmx_endpoint(
                code = "1",
                file.code = "1"
              )
              ctr <- pmx_nlmixr(fit, dvid = "TIME", conts = c("cl", "v"), endpoint = ep)
              expect_identical(ctr$endpoint$code, "1")
              expect_identical(ctr$dvid, "TIME")
            })

  test_that("pmx_nlmixr: params: fit, dvid, conts, cats, strats, ect.
            result: identical by default", {
              ctr <- pmx_nlmixr(fit, conts = c("cl", "v"), vpc = FALSE)

              expect_identical(ctr$config$sys, "nlmixr")
              expect_true(inherits(ctr$config, "pmxConfig"))
              expect_true(is.null(ctr$endpoint))
              expect_true(is.null(ctr$sim))
            })

  test_that("pmx_nlmixr: params: fit, dvid, conts, cats, strats, ect.
            result: identical conts", {
              ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))
              expect_identical(ctr$conts, c("cl", "v"))
            })


  test_that("pmx_nlmixr: params: fit, dvid, conts, cats, strats, ect.
            result:  pmxClass, R6", {
              ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))
              expect_true(inherits(ctr, c("pmxClass", "R6")))
            })

  test_that("pmx_nlmixr: params: NULL result:  error is_pmxclass(ctr) not TRUE", {
    ctr <- pmx_nlmixr()
    expect_error(ctr %>% pmx_plot_individual())
  })

  test_that("pmx_nlmixr: params: NULL result: NULL", {
    ctr <- pmx_nlmixr()
    expect_true(is.null(ctr))
  })

  test_that("pmx_nlmixr: params: conts result: NULL (fit is missing)", {
    ctr <- pmx_nlmixr(conts = c("cl", "v"))
    expect_true(is.null(ctr))
  })

  test_that("pmx_nlmixr: params: fit result: default values of the arguments ", {
    ctr <- pmx_nlmixr(fit)
    settings <- pmx_settings()

    expect_identical(ctr$cats, "")
    expect_identical(ctr$conts, "")
    expect_identical(ctr$strats, "")
    expect_identical(ctr$dvid, "")
    expect_identical(ctr$endpoint, NULL)
    expect_identical(ctr$settings, settings)
    expect_identical(ctr$sim, NULL)
  })
  test_that("pmx_nlmixr: class settings is not a pmxSettingsClass result: settings by default", {
    ctr <- pmx_nlmixr(fit, settings <- list(is.draft = FALSE))
    p_settings <- pmx_settings()
    expect_identical(ctr$settings, p_settings)
  })

  test_that("pmx_nlmixr: params: fit result: sim by default", {
    ctr <- pmx_nlmixr(fit)
    expect_identical(ctr$sim, NULL)
  })

  test_that("pmx_nlmixr: params: fit, endpoint result: error can not filter by endpoint 1
  is not a valid column", {
    ep <- pmx_endpoint(
      code = "1",
      file.code = "1"
    )
    expect_error(pmx_nlmixr(fit, endpoint = ep))
  })

  test_that("pmx_nlmixr: fit, dvid, endpoint = NULL(by default); result: error
          select a single endpoint", {
            expect_error(pmx_nlmixr(fit, dvid = "TIME"))
          })

  test_that("pmx_nlmixr: fit, dvid, endpoint; result: error
          No observations data for endpoint 3", {
            ep <- pmx_endpoint(
              code = "3",
              file.code = "1"
            )
            expect_error(ctr <- pmx_nlmixr(fit, dvid = "TIME", endpoint = ep))
          })
}
#------------------- pmx_nlmixr - end ---------------------------------------

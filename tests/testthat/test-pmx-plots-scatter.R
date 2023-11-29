if (helper_skip()) {

  context("Test Scatter residual plots")
  ctr <- theophylline()

  #------------------- pmx_plot_dv_pred start -----------------------------------
  test_that("pmx_plot_dv_pred: params: controller result: gg, ggplot", {
    expect_true(inherits(pmx_plot_dv_pred(ctr = ctr), c("gg", "ggplot")))
  })

  test_that("pmx_plot_dv_pred: params: no result: error", {
    expect_error(pmx_plot_dv_pred())
  })

  test_that("pmx_plot_dv_pred: params: not controller result: error", {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_dv_pred(ctr = ctr))
  })


  test_that("pmx_plot_dv_pred: params: ctr; result: list", {
    p <- pmx_plot_dv_pred(ctr)
    expect_true(inherits(p$scales$scales, "list"))
  })

  test_that("pmx_plot_dv_pred: params: ctr; result: identical inherist of has_scale", {
    p <- pmx_plot_dv_pred(ctr)
    expect_true(inherits(p$scales$has_scale, "ggproto_method"))
  })

  test_that("pmx_plot_dv_pred: params: ctr_mlx; result: identical scales inherits", {
    mlxpath <- file.path(
      system.file(package = "ggPMX"),
      "testdata",
      "1_popPK_model",
      "project.mlxtran"
    )
    ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
    p <- pmx_plot_dv_pred(ctr_mlx)
    expect_true(inherits(p$scales$scales, "list"))
  })


  test_that("pmx_plot_dv_pred: params: ctr, ylim; result: identical structure", {
    p <- pmx_plot_dv_pred(ctr) + ylim(-5, 5)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-5, 5)
    )
  })

  test_that("pmx_plot_dv_pred: params: not controller result: error",
  {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_dv_pred(ctr = ctr))
  })


  test_that("pmx_plot_dv_pred: params: ctrl result: x and y axes equal by default",
  {
    ctr <- theophylline()
    p <- pmx_plot_dv_pred(ctr)
    expect_equal(
      p[["coordinates"]][["limits"]][["x"]],
      p[["coordinates"]][["limits"]][["y"]]
    )
  }
  )

  #------------------- pmx_plot_dv_pred end -------------------------------------

  #------------------- pmx_plot_iwres_time start --------------------------------
  test_that("pmx_plot_iwres_time: params: controller result: gg, ggplot", {
    expect_true(inherits(pmx_plot_iwres_time(ctr = ctr), c("gg", "ggplot")))
  })

  test_that("pmx_plot_iwres_time: params: no result: error", {
    expect_error(pmx_plot_iwres_time())
  })

  test_that("pmx_plot_iwres_time: params: not controller result: error", {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_iwres_time(ctr = ctr))
  })


  test_that("pmx_plot_iwres_time: params: ctr; result: list", {
    p <- pmx_plot_iwres_time(ctr)
    expect_true(inherits(p$scales$scales, "list"))
  })

  test_that("pmx_plot_iwres_time: params: ctr; result: identical structure", {
    p <- pmx_plot_iwres_time(ctr)
    expect_identical(p$scales$scales[[1]]$limits, c(-3.3237, 3.3237))
  })

  test_that("pmx_plot_iwres_time: params: ctr_mlx; result: identical structure", {
    mlxpath <- file.path(
      system.file(package = "ggPMX"),
      "testdata",
      "1_popPK_model",
      "project.mlxtran"
    )
    ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
    p <- pmx_plot_iwres_time(ctr_mlx)
    expect_identical(p$scales$scales[[1]]$limits, c(-3.7749, 3.7749))
  })


  test_that("pmx_plot_iwres_time: params: ctr, ylim; result: identical structure", {
    p <- pmx_plot_iwres_time(ctr) + ylim(-5, 5)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-5, 5)
    )
  })


  #------------------- pmx_plot_iwres_time end ----------------------------------

  #------------------- pmx_plot_npde_time start ---------------------------------
  test_that("pmx_plot_npde_time: params: controller result: gg", {
    expect_true(inherits(pmx_plot_npde_time(ctr = ctr), "gg"))
  })

  test_that("pmx_plot_npde_time: params: no result: error", {
    expect_error(pmx_plot_npde_time())
  })

  test_that("pmx_plot_npde_time: params: not controller result: error", {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_npde_time(ctr = ctr))
  })


  test_that("pmx_plot_npde_time: params: ctr, explicit filter; result: identical type", {
    p <- ctr %>% pmx_plot_npde_time(filter = "STUD == 1")
    expect_true(inherits(p, "ggplot"))
  })


  test_that("pmx_plot_npde_time: params: ctr, implicit filter; result: identical type", {
    filter_string <- "STUD == 1"
    p <- ctr %>% pmx_plot_npde_time(filter = filter_string)

    expect_true(inherits(p, "ggplot"))
  })

  #------------------- pmx_plot_npde_time end -----------------------------------

  #------------------- pmx_plot_npde_pred start ---------------------------------
  test_that("pmx_plot_npde_pred: params: controller result: gg, ggplot", {
    expect_true(inherits(pmx_plot_npde_pred(ctr = ctr), c("gg", "ggplot")))
  })

  test_that("pmx_plot_npde_pred: params: no result: error", {
    expect_error(pmx_plot_npde_pred())
  })

  test_that("pmx_plot_npde_pred: params: not controller result: error", {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_npde_pred(ctr = ctr))
  })

  test_that("pmx_plot_npde_pred: params: ctr; result: list", {
    p <- pmx_plot_npde_pred(ctr)
    expect_true(inherits(p$scales$scales, "list"))
  })

  test_that("pmx_plot_npde_pred: params: ctr; result: identical structure", {
    p <- pmx_plot_npde_pred(ctr)
    expect_identical(p$scales$scales[[1]]$limits, c(-3.934, 3.934))
  })

  test_that("pmx_plot_npde_pred: params: ctr_mlx; result: identical scales inherits", {
    mlxpath <- file.path(
      system.file(package = "ggPMX"),
      "testdata",
      "1_popPK_model",
      "project.mlxtran"
    )
    ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
    p <- pmx_plot_npde_pred(ctr_mlx)
    expect_true(inherits(p$scales$scales, "list"))
  })


  test_that("pmx_plot_npde_pred: params: ctr, ylim; result: identical structure", {
    p <- pmx_plot_npde_pred(ctr) + ylim(-5, 5)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-5, 5)
    )
  })

  #------------------- pmx_plot_npde_pred end -----------------------------------

  #------------------- pmx_plot_abs_iwres_ipred start ---------------------------

  test_that("pmx_plot_abs_iwres_ipred: params: controller result: gg, ggplot", {
    expect_true(inherits(pmx_plot_abs_iwres_ipred(ctr = ctr), c("gg", "ggplot")))
  })

  test_that("pmx_plot_abs_iwres_time: params: controller result: gg", {
    ctr <- theophylline()
    expect_true(inherits(pmx_plot_abs_iwres_time(ctr = ctr), "gg"))
  })

  test_that("pmx_plot_abs_iwres_ipred: params: no result: error", {
    expect_error(pmx_plot_abs_iwres_ipred())
  })

  test_that(
    "pmx_plot_abs_iwres_ipred: params: not controller result: error",
    {
      ctr <- theophylline() %>% get_data("eta")
      expect_error(pmx_plot_abs_iwres_ipred(ctr = ctr))
    }
  )


  test_that("pmx_plot_abs_iwres_ipred: params: ctr; result: list", {
    p <- pmx_plot_abs_iwres_ipred(ctr)
    expect_true(inherits(p$scales$scales, "list"))
  })

  test_that("pmx_plot_abs_iwres_ipred: params: ctr; result: identical structure", {
    p <- pmx_plot_abs_iwres_ipred(ctr)
    expect_identical(p$scales$scales, list())
  })

  test_that("pmx_plot_abs_iwres_ipred: params: ctr_mlx; result: identical structure", {
    mlxpath <- file.path(
      system.file(package = "ggPMX"),
      "testdata",
      "1_popPK_model",
      "project.mlxtran"
    )
    ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
    p <- pmx_plot_abs_iwres_ipred(ctr_mlx)
    expect_identical(p$scales$scales, list())
  })


  test_that("pmx_plot_abs_iwres_ipred: params: ctr, ylim; result: identical structure", {
    p <- pmx_plot_abs_iwres_ipred(ctr) + ylim(-5, 5)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-5, 5)
    )
  })


  #------------------- pmx_plot_abs_iwres_ipred end ----------------------------

  #------------------- pmx_plot_iwres_ipred start -------------------------------
  test_that("pmx_plot_iwres_ipred: params: controller result: gg, ggplot", {
    expect_true(inherits(pmx_plot_iwres_ipred(ctr = ctr), c("gg", "ggplot")))
  })

  test_that("pmx_plot_iwres_ipred: params: no result: error", {
    expect_error(pmx_plot_iwres_ipred())
  })

  test_that(
    "pmx_plot_iwres_ipred: params: not controller result: error",
    {
      ctr <- theophylline() %>% get_data("eta")
      expect_error(pmx_plot_iwres_ipred(ctr = ctr))
    }
  )

  test_that("pmx_plot_iwres_ipred: params: ctr; result: list", {
    p <- pmx_plot_iwres_ipred(ctr)
    expect_true(inherits(p$scales$scales, "list"))
  })

  test_that("pmx_plot_iwres_ipred: params: ctr; result: identical structure", {
    p <- pmx_plot_iwres_ipred(ctr)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-3.3237, 3.3237)
    )
  })

  test_that("pmx_plot_iwres_ipred: params: ctr_mlx; result: identical structure", {
    mlxpath <- file.path(
      system.file(package = "ggPMX"),
      "testdata",
      "1_popPK_model",
      "project.mlxtran"
    )
    ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
    p <- pmx_plot_iwres_ipred(ctr_mlx)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-3.7749, 3.7749)
    )
  })


  test_that("pmx_plot_iwres_ipred: params: ctr, ylim; result: identical structure", {
    p <- pmx_plot_iwres_ipred(ctr) + ylim(-5, 5)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-5, 5)
    )
  })


  #------------------- pmx_plot_iwres_ipred end --------------------------------

  #------------------- pmx_plot_dv_ipred start ---------------------------------

  test_that("pmx_plot_dv_ipred: params: ctr; result: ggplot", {
    expect_true(inherits(pmx_plot_dv_ipred(ctr), "ggplot"))
  })

  test_that("pmx_plot_dv_ipred: params: ctr; result: list", {
    p <- pmx_plot_dv_ipred(ctr)
    expect_true(inherits(p$scales$scales, "list"))
  })

  test_that("pmx_plot_dv_ipred: params: ctr; result: identical inherist of has_scale", {
    p <- pmx_plot_dv_ipred(ctr)
    expect_true(inherits(p$scales$has_scale, "ggproto_method"))
  })

  test_that("pmx_plot_dv_ipred: params: ctr_mlx; result: identical scales inherits", {
    mlxpath <- file.path(
      system.file(package = "ggPMX"),
      "testdata",
      "1_popPK_model",
      "project.mlxtran"
    )
    ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
    p <- pmx_plot_dv_ipred(ctr_mlx)
    expect_true(inherits(p$scales$scales, "list"))
  })


  test_that("pmx_plot_dv_ipred: params: ctr, ylim; result: identical structure", {
    p <- pmx_plot_dv_ipred(ctr) + ylim(-5, 5)
    expect_identical(
      p$scales$scales[[1]]$limits,
      c(-5, 5)
    )
  })


  #------------------- pmx_plot_dv_ipred end ------------------------------------
}

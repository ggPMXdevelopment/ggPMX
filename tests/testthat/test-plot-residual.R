if (helper_skip()) {

  context("Test residual function")

  ctr <- theophylline()

  #------------------- pmx_plot_iwres_ipred start ------------------------------
  test_that("residual: params: x equals IWRES, y equals IPRED;
          result: identical structure",
  {
    x <- "IWRES"
    y <- "IPRED"
    aess <- list(x = x, y = y)
    labels <- list(
      title = paste(rev(aess), collapse = " versus "),
      subtitle = "",
      x = aess[["x"]],
      y = aess[["y"]]
    )
    expect_identical(residual(x, y),
                     structure(
                       list(
                         ptype = "SCATTER",
                         strat = TRUE,
                         dname = "predictions",
                         aess = aess,
                         point = list(
                           shape = 1,
                           colour = "black",
                           size = 1
                         ),
                         is.hline = FALSE,
                         hline = list(yintercept = 0),
                         facets = NULL,
                         bloq = NULL,
                         square_plot = TRUE,
                         gp = pmx_gpar(labels = labels)
                       ),
                       class = c("residual", "pmx_gpar")
                     ))
  })

  #------------------- residual start ------------------------------------------

  test_that("residual: params: x, y; result: error x, y is missing ", {
    x <- "IWRES"
    y <- "IPRED"
    expect_error(residual(y))
    expect_error(residual(x))
  })

  test_that("residual: params: x, y, ect.; result: error labels, point, hline are not list ot NULL ", {
    x <- "IWRES"
    y <- "IPRED"
    expect_error(residual(x, y, labels = 1))
    expect_error(residual(x, y, point = 1))
    expect_error(residual(x, y, hline = TRUE))
  })

  test_that("residual: params: x, y, ect.; result: error dname is not string ot NULL ", {
    x <- "IWRES"
    y <- "IPRED"
    expect_error(residual(x, y, dname = 1))
  })

  test_that("residual: params: x, y, dname = NULL; result: identical structure", {
    x <- "IWRES"
    y <- "IPRED"
    default_point <- list(shape = 1, colour = "black", size = 1)
    res <- residual(x, y)
    expect_identical(res$dname, "predictions")
    expect_identical(res$point, default_point)
  })

  test_that("residual: params: x, y; result: identical inherits", {
    x <- "IWRES"
    y <- "IPRED"
    res <- residual(x, y)
    expect_true(inherits(res, c("residual", "pmx_gpar")))
  })

  test_that("residual: params: x, y; result: identical names", {
    x <- "IWRES"
    y <- "IPRED"
    res <- residual(x, y)
    resNames <- c(
      "ptype", "strat", "dname", "aess", "point", "is.hline",
      "hline", "facets", "bloq", "square_plot", "gp"
    )
    expect_identical(names(res), resNames)
  })


  #------------------- residual end ------------------------------------------

  #------------------- extend_range start ------------------------------------

  test_that("extend_range: params: x; result: identical range", {
    dx <- ctr %>% get_data("omega")
    suppressWarnings({
      dx_range <- extend_range(x = dx)
    })
    expect_identical(dx_range, c(Inf, -Inf))

  })

  test_that("extend_range: params: x; result: error 'r' must be a 'range', hence of length 2", {
    dx <- ctr %>% get_data("omega")
    expect_error(extend_range(x = dx, r = Inf))
  })

  test_that("extend_range: params: NULL; result: error missing arguments", {
    expect_error(extend_range())
  })

  test_that("extend_range: params: x; result: error data frame should has all numeric variables", {
    dx <- ctr %>% get_data("eta")
    dx <- dx[, EFFECT := factor(
      EFFECT,
      levels = c("ka", "V", "Cl"),
      labels = c("Concentration", "Volume", "Clearance")
    )]
    expect_error(extend_range(x = dx[, c(aess$x, aess$y), with = FALSE]))
  })

  #------------------- extend_range end --------------------------------------

  #------------------- plot_pmx.residual start -------------------------------

  test_that("plot_pmx.residual: params: NULL; result: error missing arguments", {
    expect_error(plot_pmx.residual())
  })

  test_that("plot_pmx.residual: params: x, dx; result: NULL", {
    x <- "IWRES"
    y <- "IPRED"
    dx <- ctr %>% get_data("eta")
    dx <- dx[, EFFECT := factor(
      EFFECT,
      levels = c("ka", "V", "Cl"),
      labels = c("Concentration", "Volume", "Clearance")
    )]
    res <- residual(x, y)
    expect_identical(plot_pmx.residual(x = res, dx), NULL)
  })

  test_that("plot_pmx.residual: params: x, dx; result: identical structure", {
    x <- "STUD"
    y <- "SEX"
    dx <- ctr %>% get_data("eta")
    dx <- dx[, EFFECT := factor(
      EFFECT,
      levels = c("ka", "V", "Cl"),
      labels = c("Concentration", "Volume", "Clearance")
    )]
    bloq <- pmx_bloq(cens = "EVID")
    bloq$show <- NULL
    res <- residual(x, y, is.hline = TRUE, bloq = bloq)
    pl_resid <- plot_pmx.residual(x = res, dx)
    expect_identical(pl_resid$bloq$cens, NULL)
    expect_identical(pl_resid$bloq$limit, NULL)
    expect_identical(pl_resid$bloq$cens, NULL)
    expect_identical(pl_resid$is.hline, NULL)
  })


  test_that("plot_pmx.residual: params: x, dx, res$gp$scale_x_log10,  scale_x_log10
          are not NULL; result: identical inherits", {
            x <- "Y"
            y <- "DV"
            dx <- ctr %>% get_data("eta")

            aess <- list(x = "Y", y = "DV")
            res <- residual(x, y, ranges = list(x = c(0, 500)), is.hline = TRUE)
            res$aess$y <- "DV"
            res$gp$scale_x_log10 <- F
            res$gp$scale_y_log10 <- F
            res$gp$ranges$x <- NULL
            res$gp$ranges$y <- NULL
            pl_resid <- plot_pmx.residual(x = res, dx)
            expect_true(is_ggplot(pl_resid))
          })

  test_that("plot_pmx.residual: params: x, dx, res$ranges$x is not NULL; result: identical inherits", {
    x <- "Y"
    y <- "DV"
    dx <- ctr %>% get_data("eta")

    aess <- list(x = "Y", y = "DV")
    res <- residual(x, y, ranges = list(x = c(0, 500)), is.hline = TRUE)
    res$aess$y <- "DV"
    res$gp$scale_x_log10 <- F
    res$gp$scale_y_log10 <- F
    pl_resid <- plot_pmx.residual(x = res, dx)
    expect_true(is_ggplot(pl_resid))
  })

  test_that("plot_pmx.residual: params: x, dx, res$strat.facet, res$strat.color;
          result: identical inherits", {
            x <- "Y"
            y <- "DV"
            dx <- ctr %>% get_data("eta")

            aess <- list(x = "Y", y = "DV")
            res <- residual(x, y, ranges = list(x = c(0, 500), y = c(0, 100)), is.hline = TRUE)
            res$aess$y <- "DV"
            res$gp$scale_x_log10 <- F
            res$gp$scale_y_log10 <- F
            res$strat.color <- "SEX"
            res$strat.facet <- "STUD"
            pl_resid <- plot_pmx.residual(x = res, dx)
            expect_true(is_ggplot(pl_resid))

          })

  #------------------- plot_pmx.residual end ---------------------------------

  test_that("pmx_plot_iwres_ipred: params: ctr; result: ggplot", {
    expect_true(is_ggplot(pmx_plot_iwres_ipred(ctr)))
  })

  test_that("pmx_plot_iwres_ipred: params: ctr; result: list", {
    p <- pmx_plot_iwres_ipred(ctr)
    expect_true(inherits(p$scales$scales, "list"))
  })

  test_that(
    "pmx_plot_iwres_ipred: params: ctr; result: identical structure",
    {
      p <- pmx_plot_iwres_ipred(ctr)
      expect_identical(
        p$scales$scales[[1]]$limits,
        c(-3.3237, 3.3237)
      )
    }
  )

  test_that(
    "pmx_plot_iwres_ipred: params: ctr_mlx; result: identical structure",
    {
      mlxpath <- file.path(
        system.file(package = "ggPMX"),
        "testdata",
        "1_popPK_model",
        "project.mlxtran"
      )
      suppressWarnings({
        ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
      })

      p <- pmx_plot_iwres_ipred(ctr_mlx)
      expect_identical(
        p$scales$scales[[1]]$limits,
        c(-3.7749, 3.7749)
      )
    }
  )

  test_that("pmx_plot_iwres_ipred: params: strat.facet as formula/character
          result: plot panels", {
            ctr <- theophylline()
            p_formula <- pmx_plot_iwres_ipred(ctr, strat.facet = "SEX")
            expect_equal(levels(ggplot_build(p_formula)[[1]][[1]][["PANEL"]]), c("1", "2"))
            p_char <- pmx_plot_iwres_ipred(ctr, strat.facet = ~SEX)
            expect_equal(levels(ggplot_build(p_char)[[1]][[1]][["PANEL"]]), c("1", "2"))
            p_non_ex <- pmx_plot_iwres_ipred(ctr, strat.facet = ~4)
            expect_equal(levels(ggplot_build(p_non_ex)[[1]][[1]][["PANEL"]]), c("1"))
          })

  test_that(
    "pmx_plot_iwres_ipred: params: ctr, ylim; result: identical structure",
    {
      p <- pmx_plot_iwres_ipred(ctr) + ylim(-5, 5)
      expect_identical(
        p$scales$scales[[1]]$limits,
        c(-5, 5)
      )
    }
  )


  #------------------- pmx_plot_iwres_ipred end --------------------------------

  #------------------- pmx_plot_npde_time start --------------------------------

  test_that(
    "pmx_plot_npde_time: params: ctr, explicit filter; result: identical type",
    {
      p <- ctr %>% pmx_plot_npde_time(filter = "STUD == 1")

      expect_true(is_ggplot(p))
    }
  )


  test_that(
    "pmx_plot_npde_time: params: ctr, implicit filter; result: identical type",
    {
      filter_string <- "STUD == 1"
      p <- ctr %>% pmx_plot_npde_time(filter = filter_string)

      expect_true(is_ggplot(p))

    }
  )

  #------------------- pmx_plot_npde_time end ----------------------------------

  #------------------- pmx_plot_cats start -------------------------------------

  test_that(
    "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
    {
      p <- ctr %>% pmx_plot_cats("dv_pred",
                                 strat.facet = ~STUD,
                                 facets = list(nrow = 2, ncol = 1)
                                 )

      expect_identical(p[[1]]$facet$params$nrow, 2)
      expect_identical(p[[1]]$facet$params$ncol, 1)
    }
  )

  test_that(
    "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
    {
      p <- ctr %>% pmx_plot_cats("dv_pred", strat.facet = ~STUD)

      expect_identical(p[[1]]$facet$params$nrow, NULL)
      expect_identical(p[[1]]$facet$params$ncol, NULL)
    }
  )


  test_that(
    "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
    {
      p <- ctr %>% pmx_plot_cats("pmx_vpc", strat.facet = ~STUD,
                                 facets = list(nrow = 2, ncol = 1))

      expect_identical(p[[1]]$facet$params$nrow, 2)
      expect_identical(p[[1]]$facet$params$ncol, 1)
    }
  )

  test_that(
    "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
    {
      p <- ctr %>% pmx_plot_cats("npde_time", strat.facet = ~STUD,
                                 facets = list(nrow = 2, ncol = 1))

      expect_identical(p[[1]]$facet$params$nrow, 2)
      expect_identical(p[[1]]$facet$params$ncol, 1)
    }
  )

  test_that(
    "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
    {
      p <- ctr %>% pmx_plot_cats("iwres_time", strat.facet = ~STUD,
                                 facets = list(nrow = 2, ncol = 1))

      expect_identical(p[[1]]$facet$params$nrow, 2)
      expect_identical(p[[1]]$facet$params$ncol, 1)
    }
  )

  test_that(
    "pmx_plot_dv_ipred: params: ctr, strat.color, point(...);
  result: aesthetic params applied along with strat.color",
    {
      params <- list(alpha=0.1, size=2, stroke=2, shape=23, fill="red")
      p <- do.call(pmx_plot_dv_ipred, list(ctr=ctr, strat.color="WT0", point=params))
      lapply(names(params), function(a) {
        value <- p[["plot_env"]][["point"]][[a]]
        if(inherits(value, "quosure")) {value <- as_label(value)}
        expect_identical(value, params[[a]])
      })
    }
  )
  #------------------- pmx_plot_cats end --------------------------------------


  #------------------- pmx_plot_dv_pred start -------------------------------------

  test_that(
    "pmx_plot_dv_pred: params: ctr, range; result: squared by default, with
   applied ranges with square_plot = FALSE",
    {
      ctr <- theophylline()
      p1 <- ctr %>% pmx_plot_dv_pred(ranges = list(x = c(200, 500), y = c(100, 200)))

      p2 <- ctr %>% pmx_plot_dv_pred(
        ranges = list(x = c(200, 500), y = c(100, 200)),
        square_plot = FALSE
      )

      expect_equal(
        p1[["plot_env"]][["gp"]][["ranges"]][["y"]][[2]],
        p1[["plot_env"]][["gp"]][["ranges"]][["x"]][[2]]
      )

      expect_equal(p2[["plot_env"]][["gp"]][["ranges"]][["x"]], c(200, 500))
      expect_equal(p2[["plot_env"]][["gp"]][["ranges"]][["y"]], c(100, 200))
    }
  )

  #------------------- pmx_plot_dv_pred end --------------------------------------
}

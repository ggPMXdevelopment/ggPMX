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
          gp = pmx_gpar(labels = labels)
        ),
        class = c("residual", "pmx_gpar")
      ))
  }
)

test_that("pmx_plot_iwres_ipred: params: ctr; result: ggplot", {
  expect_true(inherits(pmx_plot_iwres_ipred(ctr), "ggplot"))
})

test_that("pmx_plot_iwres_ipred: params: ctr; result: list", {
  p <- pmx_plot_iwres_ipred(ctr)
  expect_true(inherits(p$scales$scales, "list"))
})

test_that(
  "pmx_plot_iwres_ipred: params: ctr; result: identical structure",
  {
    p <- pmx_plot_iwres_ipred(ctr)
    expect_identical(p$scales$scales[[1]]$limits,
                     c(-3.3237, 3.3237))
  }
)

test_that(
  "pmx_plot_iwres_ipred: params: ctr_mlx; result: identical structure",
  {
    mlxpath <- file.path(system.file(package = "ggPMX"),
                         "testdata",
                         "1_popPK_model",
                         "project.mlxtran")
    ctr_mlx <- pmx_mlxtran(mlxpath, config = "standing")
    p <- pmx_plot_iwres_ipred(ctr_mlx)
    expect_identical(p$scales$scales[[1]]$limits,
                     c(-3.7749, 3.7749))
  }
)


test_that(
  "pmx_plot_iwres_ipred: params: ctr, ylim; result: identical structure",
  {
    p <- pmx_plot_iwres_ipred(ctr) + ylim(-5, 5)
    expect_identical(p$scales$scales[[1]]$limits,
                     c(-5, 5))
  }
)


#------------------- pmx_plot_iwres_ipred end --------------------------------

#------------------- pmx_plot_npde_time start --------------------------------

test_that(
  "pmx_plot_npde_time: params: ctr, explicit filter; result: identical type",
  {
    p <- ctr %>% pmx_plot_npde_time(filter = "STUD == 1")

    expect_true(inherits(p, "ggplot"))
  }
)


test_that(
  "pmx_plot_npde_time: params: ctr, implicit filter; result: identical type",
  {
    filter_string = "STUD == 1"
    p <- ctr %>% pmx_plot_npde_time(filter = filter_string)

    expect_true(inherits(p, "ggplot"))
  }
)

#------------------- pmx_plot_npde_time end ----------------------------------

#------------------- pmx_plot_cats start -------------------------------------

test_that(
  "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
  {
    p <- ctr %>% pmx_plot_cats("dv_pred", strat.facet = 'STUD',
      facets = list(nrow = 2, ncol = 1))

    expect_identical(p[[1]]$facet$params$nrow, 2)
    expect_identical(p[[1]]$facet$params$ncol, 1)

  }
)

test_that(
  "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
  {
    p <- ctr %>% pmx_plot_cats("dv_pred", strat.facet = 'STUD')

    expect_identical(p[[1]]$facet$params$nrow, NULL)
    expect_identical(p[[1]]$facet$params$ncol, NULL)
  }
)


test_that(
  "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
  {
    p <- ctr %>% pmx_plot_cats("pmx_vpc", strat.facet = 'STUD',
      facets = list(nrow = 2, ncol = 1))

    expect_identical(p[[1]]$facet$params$nrow, 2)
    expect_identical(p[[1]]$facet$params$ncol, 1)
  }
)

test_that(
  "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
  {
    p <- ctr %>% pmx_plot_cats("npde_time", strat.facet = 'STUD',
      facets = list(nrow = 2, ncol = 1))

    expect_identical(p[[1]]$facet$params$nrow, 2)
    expect_identical(p[[1]]$facet$params$ncol, 1)
  }
)

test_that(
  "pmx_plot_cats: params: ctr; result: identical numbers of columns and rows",
  {
    p <- ctr %>% pmx_plot_cats("iwres_time", strat.facet = 'STUD',
      facets = list(nrow = 2, ncol = 1))

    expect_identical(p[[1]]$facet$params$nrow, 2)
    expect_identical(p[[1]]$facet$params$ncol, 1)
  })

#------------------- pmx_plot_cats end --------------------------------------


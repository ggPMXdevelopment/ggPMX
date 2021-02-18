context("Test residual function")

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
          })

test_that("pmx_plot_iwres_ipred: params: ctr; result: ggplot", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_iwres_ipred(ctr), "ggplot"))
})

test_that("pmx_plot_iwres_ipred: params: ctr; result: list", {
  ctr <- theophylline()
  p <- pmx_plot_iwres_ipred(ctr)
  expect_true(inherits(p$scales$scales, "list"))
})

test_that("pmx_plot_iwres_ipred: params: ctr; result: identical structure",
          {
            ctr <- theophylline()
            p <- pmx_plot_iwres_ipred(ctr)
            expect_identical(p$scales$scales[[1]]$limits,
                             c(-3.3237, 3.3237))
          })

test_that("pmx_plot_iwres_ipred: params: ctr; result: identical structure",
          {
            mlxpath <- file.path(system.file(package = "ggPMX"),
                                 "testdata",
                                 "1_popPK_model",
                                 "project.mlxtran")
            ctr <- pmx_mlxtran(mlxpath, config = "standing")
            p <- pmx_plot_iwres_ipred(ctr)
            expect_identical(p$scales$scales[[1]]$limits,
                             c(-3.7749, 3.7749))
          })

test_that("pmx_plot_iwres_ipred: params: ctr, ylim; result: identical structure",
          {
            ctr <- theophylline()
            p <- pmx_plot_iwres_ipred(ctr) + ylim(-5, 5)
            expect_identical(p$scales$scales[[1]]$limits,
                             c(-5, 5))
          })

#------------------- pmx_plot_iwres_ipred end --------------------------------

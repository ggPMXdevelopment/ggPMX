if (helper_skip()) {

  context("Test pmx_dens function")

  #------------------- pmx_dens start ------------------------------------------
  test_that("pmx_dens: params: x equals ETA_COV; result: identical structure", {
    x <- "ETA_COV"
    expect_identical(
      pmx_dens(x),
      structure(
        list(
          ptype = "PMX_DENS",
          strat = TRUE,
          x = "ETA_COV",
          dname = "predictions",
          xlim = 3,
          var_line = list(
            linetype = 1,
            colour = "black",
            linewidth = 1
          ),
          snd_line = list(
            linetype = 2,
            colour = "black",
            linewidth = 1
          ),
          vline = list(
            linetype = 2,
            colour = "black",
            linewidth = 1
          ),
          is.legend = TRUE,
          gp = pmx_gpar(
            labels = list(
              title = sprintf("Density plot of %s", x),
              y = "",
              x = "",
              subtitle = ""
            ),
            discrete = TRUE,
            is.smooth = FALSE
          )
        ),
        class = c("pmx_dens", "pmx_gpar")
      )
    )
  })

  test_that("pmx_dens: params: x equals ETA_COV; result: pmx_dens", {
    x <- "ETA_COV"
    expect_true(inherits(pmx_dens(x = x), "pmx_dens"))
  })

  test_that("pmx_dens: params: x equals NULL; result: pmx_dens", {
    x <- NULL
    expect_true(inherits(pmx_dens(x = x), "pmx_dens"))
  })

  test_that("pmx_dens: params: integer dname; result: error", {
    dname <- 10L
    expect_error(inherits(pmx_dens(dname = dname), "pmx_dens"))
  })

  test_that("pmx_dens: params: x equals ETA_COV, dname is NULL; result: pmx_dens", {
    x <- "ETA_COV"
    dname <- NULL
    expect_true(inherits(pmx_dens(x = x, dname = dname), "pmx_dens"))
  })

  test_that("pmx_dens: params: x is NULL, dname is NULL; result: pmx_dens", {
    x <- NULL
    dname <- NULL
    expect_true(inherits(pmx_dens(x = x, dname = dname), "pmx_dens"))
  })

  test_that("pmx_dens: params: labels is list, x isn't provided; result: error", {
    labels <- list(
      title = sprintf("Density plot"),
      y = "",
      x = "",
      subtitle = ""
    )
    expect_error(inherits(pmx_dens(labels = labels), "pmx_dens"))
  })

  test_that("pmx_dens: params: labels is list, x is provided; result: pmx_dens", {
    x <- "ETA"
    labels <- list(
      title = sprintf("Density plot"),
      y = "",
      x = "",
      subtitle = ""
    )
    expect_true(inherits(pmx_dens(x = x, labels = labels), "pmx_dens"))
  })

  test_that("pmx_dens: params: integer labels; result: error", {
    labels <- 10L
    expect_error(inherits(pmx_dens(labels = labels), "pmx_dens"))
  })

  test_that("pmx_dens: params: labels character; result: error", {
    labels <- "test label"
    expect_error(inherits(pmx_dens(labels = labels), "pmx_dens"))
  })
  #------------------- pmx_dens end --------------------------------------------

  #------------------- pmx_plot_iwres_dens start -------------------------------
  mlxpath <- file.path(
    system.file(package = "ggPMX"),
    "testdata",
    "1_popPK_model",
    "project.mlxtran"
  )
  ctr <- pmx_mlxtran(mlxpath, config = "standing")

  test_that("pmx_dens: params: ctr; result: ggplot", {
    expect_true(inherits(pmx_plot_iwres_dens(ctr), "ggplot"))
  })

  test_that("pmx_dens: params: ctr is NULL; result: ggplot", {
    expect_error(pmx_plot_iwres_dens(ctr = NULL))
  })

  test_that("pmx_dens: params: ctr, is.legend is FALSE; result: ggplot", {
    expect_true(inherits(pmx_plot_iwres_dens(ctr, is.legend = FALSE), "ggplot"))
  })

  test_that("pmx_dens: params: ctr, var_line; result: ggplot", {
    expect_true(inherits(
      pmx_plot_iwres_dens(
        ctr,
        var_line = list(
          colour = "green",
          size = 1,
          linetype = 1
        )
      ),
      "ggplot"
    ))
  })


  test_that("pmx_dens: params: ctr; result: plot is not blank", {
    ctr <- theophylline()
    p <- pmx_plot_iwres_dens(ctr)
    p_obj <- ggplot2::ggplot_build(p)[["data"]][1:2]
    expect_false((length(p_obj[[1]]) == 0) || (length(p_obj[[2]]) == 0))
  })


  test_that("pmx_dens: params: ctr, var_line, snd_line; result: ggplot", {
    expect_true(inherits(
      pmx_plot_iwres_dens(
        ctr,
        var_line = list(
          colour = "green",
          size = 1,
          linetype = 1
        ),
        snd_line = list(colour = "red", size = 1)
      ),
      "ggplot"
    ))
  })
  #------------------- pmx_plot_iwres_dens end ---------------------------------
}

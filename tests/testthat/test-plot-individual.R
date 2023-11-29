if (helper_skip()) {

  library(ggPMX)
  library(ggplot2)

  context("Test plot_pmx.individual function")
  mlxpath <- file.path(
    system.file(package = "ggPMX"),
    "testdata",
    "1_popPK_model",
    "project.mlxtran"
  )
  ctr <- pmx_mlxtran(mlxpath, config = "standing")

  #------------------- pmx_plot_individual start -------------------------------
  test_that("pmx_plot_individual: params: no; result: error ctr is missing", {
    expect_error(pmx_plot_individual())
  })

  test_that("pmx_plot_individual: params: ctr, point; result: identical names", {
    indiv_plot <- pmx_plot_individual(ctr, point = list(
      colour = c("black", "green")
    ))
    indNames <- c("data", "layers", "scales", "mapping", "theme", "coordinates", "facet", "plot_env", "labels")
    expect_identical(names(indiv_plot), indNames)
  })


  test_that("pmx_plot_individual: params: no; result: ggplot", {
    expect_true(inherits(pmx_plot_individual(ctr), "ggplot"))
  })


  test_that("pmx_plot_individual: params: ctr, which_pages; result: error which_pages is not an integer or 'all'", {
    expect_error(pmx_plot_individual(ctr, which_pages = c("all", "plot")))
  })

  test_that("pmx_plot_individual: params: ctr, which_pages; result: error  class ctr is not pmxclass", {
    ctr <- ""
    expect_error(pmx_plot_individual(ctr))
  })

  test_that("pmx_plot_individual: params: ctr, which_pages, dname; result: error
          individual is not a valid plot name", {
            expect_error(pmx_plot_individual(ctr, which_pages = "all", dname = "IND1"))
          })

  test_that("pmx_plot_individual: params: ctr, which_pages, dname; result: warning
          individual is not a valid plot name", {
            expect_warning(pmx_plot_individual(ctr, which_pages = "all", dname = "IND", npage = 1))
          })


  test_that("pmx_plot_individual: params: point; result: ggplot", {
    expect_true(inherits(pmx_plot_individual(ctr, point = list(
      colour = c("black", "green")
    )), "ggplot"))
  })


  test_that("pmx_plot_individual : doesn't have NA panels after stratifying", {
    ctr <- theophylline()
    # creating some NA data for testing purposes
    ctr[["data"]][["IND"]][["SEX"]][which(ctr[["data"]][["IND"]][["SEX"]] == 1)] <- NA
    # testing both formula and character class strat.facet arguments
    lapply(
      list(
        pmx_plot_individual(ctr, strat.facet=~SEX),
        pmx_plot_individual(ctr, strat.facet="SEX")
      ),
      function(p) {
        built_plot <- ggplot2::ggplot_build(p)
        expect_equal(0,
                     sum(is.na(ggplot2::ggplot_build(p)[["layout"]][["layout"]][["SEX"]]))
                     )
      }
    )
  })


  test_that("pmx_plot_individual: params: point (colour and shape); result: ggplot", {
    expect_true(inherits(pmx_plot_individual(ctr, point = list(
      colour = "blue", shape = 24
    )), "ggplot"))
  })

  test_that("plot_pmx.individual: params: point and pred_line; result: ggplot", {
    expect_true(inherits(
      pmx_plot_individual(
        ctr,
        bloq = pmx_bloq(cens = "BLOQ"),
        point = list(colour = c("blue", "red")),
        pred_line = list(color = "red", alpha = 0.5),
        which_pages = 1
      ),
      "ggplot"
    ))
  })
  ###
  test_that("plot_pmx.individual: params: is.legend is FALSE; result: ggplot", {
    expect_true(inherits(
      pmx_plot_individual(
        ctr,
        is.legend = FALSE
      ),
      "ggplot"
    ))
  })


  test_that("plot_pmx.individual: params: point, ipred_line and pred_line;
          result: ggplot", {
            expect_true(inherits(
              pmx_plot_individual(
                ctr,
                point = list(colour = "blue", shape = 24),
                ipred_line = list(colour = "red"),
                pred_line = list(colour = "green")
              ),
              "ggplot"
            ))
          })

  test_that("plot_pmx.individual: params: ctr is theophylline; result: ggplot", {
    ctr <- theophylline()
    expect_true(inherits(
      pmx_plot_individual(
        ctr
      ),
      "ggplot"
    ))
  })


  test_that("plot_pmx.individual: params: ctr is theophylline,
           point, ipred_line and pred_line; result: ggplot", {
             ctr <- theophylline()
             expect_true(inherits(
               pmx_plot_individual(
                 ctr,
                 point = list(colour = "blue", shape = 24),
                 ipred_line = list(colour = "red"),
                 pred_line = list(colour = "green")
               ),
               "ggplot"
             ))
           })

  test_that("plot_pmx.individual: params: ctr is theophylline,
           passing arguments from parent frame; No Error", {
             ctr <- theophylline()

             expect_error(
             {
               f <- function() {
                 for (i in 1:2) {
                   print(ctr %>% pmx_plot_individual(which_pages = i, facets = list(nrow = 1, ncol = 1)))
                 }
               }

               f()
             },
             NA
             )
           })

  test_that("pmx_plot_individual: params: ctr is theophylline, point; result: identical names", {
    ctr <- theophylline()
    indiv_plot <- pmx_plot_individual(ctr, point = list(
      colour = c("black", "green")
    ))
    indNames <- c("data", "layers", "scales", "mapping", "theme", "coordinates", "facet", "plot_env", "labels")
    expect_identical(names(indiv_plot), indNames)
  })

  test_that("pmx_plot_individual: params: ctr is theophylline; result: ggplot", {
    ctr <- theophylline()
    expect_true(inherits(pmx_plot_individual(ctr), "ggplot"))
  })

  test_that("pmx_plot_individual: params: ctr is theophylline,  point; result: ggplot", {
    ctr <- theophylline()
    expect_true(inherits(pmx_plot_individual(ctr, point = list(
      colour = c("black", "green")
    )), "ggplot"))
  })

  test_that("pmx_plot_individual: params: ctr is theophylline, point (colour and shape); result: ggplot", {
    ctr <- theophylline()
    expect_true(inherits(pmx_plot_individual(ctr, point = list(
      colour = "blue", shape = 24
    )), "ggplot"))
  })

  test_that("pmx_plot_individual: params: ctr, point (colour and shape), footnote; result: identical structure ", {
    ctr$footnote <- TRUE
    indiv_plot <- pmx_plot_individual(ctr, which_pages = 1, point = list(
      colour = "blue", shape = 24
    ), ctr$footnote)
    expect_identical(indiv_plot$plot_env$p$plot_env$ptype, "IND")
  })

  test_that("pmx_plot_individual: params: ctr, which_pages, point (colour and shape), print; result: identical output ", {
    indiv_plot <- pmx_plot_individual(ctr, which_pages = 1, point = list(
      colour = "blue", shape = 24
    ), print = TRUE)
    expect_output(indiv_plot, NA)
  })

  #------------------- pmx_plot_individual end ---------------------------------

  #------------------- individual start -------------------------------
  test_that("individual: params: lables, facets, dname etc.; result: individual", {
    dx <- ctr %>% get_data("IND")
    labels <- list("DOSE")
    expect_true(inherits(
      individual(
        labels,
        facets = list(
          ncol = 3,
          nrow = 4,
          scales = "free"
        ),
        dname = "IND",
        is.legend = TRUE,
        use.finegrid = TRUE
      ),
      "individual"
    ))
  })

  test_that("individual: params: labels, facets, dname etc.; result: pmx_gpar", {
    dx <- ctr %>% get_data("IND")
    labels <- list("DOSE")
    expect_true(inherits(
      individual(
        labels,
        facets = list(
          ncol = 3,
          nrow = 4,
          scales = "free"
        ),
        dname = "IND",
        is.legend = TRUE,
        use.finegrid = TRUE
      ),
      "pmx_gpar"
    ))
  })

  test_that("individual: params: labels, facets, dname etc.; result: identical structure", {
    labels <- list("DOSE")
    ind <- individual(
      labels,
      facets = list(
        ncol = 3,
        nrow = 4,
        scales = "free"
      ),
      dname = "IND",
      is.legend = TRUE,
      use.finegrid = FALSE
    )
    expect_identical(ind$dname, "predictions")
  })

  test_that("individual: params: labels, facets, dname etc.; result: error argument facets is not a list", {
    labels <- list("DOSE")
    expect_error(individual(
      labels,
      facets = NULL,
      dname = "IND",
      is.legend = TRUE,
      use.finegrid = FALSE
    ))
  })

  test_that("individual: params: labels, facets, dname etc.; result: error argument labels is not a list", {
    labels <- "DOSE"
    expect_error(individual(
      labels,
      facets = list(
        ncol = 3,
        nrow = 4,
        scales = "free"
      ),
      dname = "IND",
      is.legend = TRUE,
      use.finegrid = FALSE
    ))
  })

  test_that("individual: params: labels, facets, dname etc.; result: error argument
          dname is not a string or NULL", {
            labels <- list("DOSE")
            expect_error(individual(
              labels,
              facets = list(
                ncol = 3,
                nrow = 4,
                scales = "free"
              ),
              dname = IND,
              is.legend = TRUE,
              use.finegrid = FALSE
            ))
          })



  test_that("individual: params: labels, facets, dname etc.; result: identical name", {
    labels <- list("DOSE")
    ind <- individual(
      labels,
      facets = list(
        ncol = 3,
        nrow = 4,
        scales = "free"
      ),
      dname = "IND",
      is.legend = TRUE,
      use.finegrid = FALSE
    )
    indNames <- c(
      "ptype", "strat", "is.legend", "use.finegrid", "dname", "aess", "labels",
      "point", "ipred_line", "pred_line", "facets", "bloq", "gp"
    )
    expect_identical(names(ind), indNames)
  })

  #------------------- plot_pmx.individual end ---------------------------------

  #------------------- add_footnote start --------------------------------------
  test_that("add_footnote: params: pp, pname, save_dir; result: identical inherits", {
    pp <- ctr %>% get_plot("individual")
    expect_true(inherits(add_footnote(pp[[1]], pname = "indiv1", save_dir = ctr$save_dir), c("gg", "ggplot")))
  })

  test_that("add_footnote: params: pp, pname, save_dir; result: identical names", {
    pp <- ctr %>% get_plot("individual")
    add_f <- add_footnote(pp[[1]], pname = "indiv1", save_dir = ctr$save_dir)
    fNames <- c("data", "layers", "scales", "mapping", "theme", "coordinates", "facet", "plot_env", "labels")
    expect_equal(names(add_f), fNames)
  })

  test_that("add_footnote: params: pp, pname, save_dir; result: identical structure", {
    pp <- ctr %>% get_plot("individual")
    add_f <- add_footnote(pp[[1]], pname = "indiv1", save_dir = ctr$save_dir)
    expect_identical(add_f$plot_env$dname, "IND")
    expect_true(add_f$plot_env$gp$is.draft)
    expect_identical(add_f$plot_env$aess$x, "TIME")
    expect_identical(add_f$labels$colour, "isobserv")
  })

  test_that("add_footnote: params result: error missing arguments", {
    pp <- ctr %>% get_plot("individual")
    expect_error(add_footnote())
    expect_error(add_footnote(pp[[1]]))
    expect_error(add_footnote(pp[[1]], pname = "indiv1"))
    expect_error(add_footnote(pp[[1]], save_dir = ctr$save_dir))
    expect_error(add_footnote(pname = "indiv1", save_dir = ctr$save_dir))
    expect_error(add_footnote(pname = "indiv1"))
    expect_error(add_footnote(save_dir = ctr$save_dir))
  })

  #------------------- add_footnote end ----------------------------------------
}

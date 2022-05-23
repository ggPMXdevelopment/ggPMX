context("Test plot_pmx.individual function")
mlxpath <- file.path(system.file(package = "ggPMX"),
                     "testdata",
                     "1_popPK_model",
                     "project.mlxtran")
ctr <- pmx_mlxtran(mlxpath, config = "standing")

#------------------- pmx_plot_individual start -------------------------------
test_that("pmx_plot_individual: params: no; result: ggplot", {
  expect_true(inherits(pmx_plot_individual(ctr), "ggplot"))
})

test_that("pmx_plot_individual: params: point; result: ggplot", {
  expect_true(inherits(pmx_plot_individual(ctr, point = list(
    colour = c("black", "green")
  )), "ggplot"))
})

test_that("pmx_plot_individual: params: point (colour and shape); result: ggplot",
          {
            expect_true(inherits(pmx_plot_individual(ctr, point = list(
              colour = "blue", shape = 24
            )), "ggplot"))
          })

test_that("plot_pmx.individual: params: point and pred_line; result: ggplot",
          {
            expect_true(inherits(
              pmx_plot_individual(
                ctr,
                bloq = pmx_bloq(cens = "BLOQ"),
                point = list(colour = c("blue", "red")),
                pred_line = list(color = 'red', alpha = 0.5),
                which_pages = 1
              ),
              "ggplot"
            ))
          })
###
test_that("plot_pmx.individual: params: is.legend is FALSE; result: ggplot",
          {
            expect_true(inherits(
              pmx_plot_individual(
                ctr,
                is.legend = FALSE
              ),
              "ggplot"
            ))
          })


test_that("plot_pmx.individual: params: point, ipred_line and pred_line; 
          result: ggplot",
          {
            expect_true(inherits(
              pmx_plot_individual(
                ctr,
                point = list(colour="blue", shape = 24),
                ipred_line = list(colour="red"),
                pred_line = list(colour="green")
              ),
              "ggplot"
            ))
          })

test_that("plot_pmx.individual: params: ctr is theophylline; result: ggplot",
          {
            ctr <- theophylline()
            expect_true(inherits(
              pmx_plot_individual(
                ctr
              ),
              "ggplot"
            ))
          })

test_that("plot_pmx.individual: params: ctr is theophylline, 
           point, ipred_line and pred_line; result: ggplot",
          {
            ctr <- theophylline()
            expect_true(inherits(
              pmx_plot_individual(
                ctr,
                point = list(colour="blue", shape = 24),
                ipred_line = list(colour="red"),
                pred_line = list(colour="green")
              ),
              "ggplot"
            ))
          })

test_that("plot_pmx.individual: params: ctr is theophylline,
           passing arguments from parent frame; No Error",
{
  ctr <- theophylline()

  expect_error(
    {
      f <-function() {
        for (i in 1:2) {
          print(ctr %>% pmx_plot_individual(npage=i, facets = list(nrow=1,ncol=1)))
        }
      }

      f()
    },
    NA
  )
})
#------------------- pmx_plot_individual end ---------------------------------

mlxpath <- file.path(system.file(package = "ggPMX"),
                     "testdata",
                     "1_popPK_model",
                     "project.mlxtran")
ctr <- pmx_mlxtran(mlxpath, config = "standing")
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
      dname = 'IND',
      is.legend = TRUE,
      use.finegrid = TRUE
    ),
    "individual"
  ))
})

test_that("individual: params: ables, facets, dname etc.; result: pmx_gpar", {
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
      dname = 'IND',
      is.legend = TRUE,
      use.finegrid = TRUE
    ),
    "pmx_gpar"
  ))
})
#------------------- plot_pmx.individual end ---------------------------------

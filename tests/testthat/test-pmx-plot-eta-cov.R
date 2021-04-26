context("Test pmx Eta Covariates plots")

#------------------- pmx_plot_eta_cats start -------------------------------
test_that("pmx_plot_eta_cats: params: ctr is controller result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr), "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, strat.color, strat.facet result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(
              pmx_plot_eta_cats(ctr = ctr, strat.color = "STUD", strat.facet =  ~ SEX),
              "gg"
            ))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, is.hline=TRUE, hline result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(
              pmx_plot_eta_cats(ctr = ctr, is.hline=TRUE, hline = 0.25),
              "gg"
            ))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, is.hline=FALSE result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   is.hline = FALSE),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, is.jitter = FALSE result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   is.jitter = FALSE),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, is.jitter = TRUE, jitter  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   is.jitter = TRUE, 
                                                   jitter = list(shape = 24, 
                                                                 colour = 'red', 
                                                                 size =2)),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, is.jitter = TRUE, jitter is NULL  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   is.jitter = TRUE, 
                                                   jitter = NULL),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, is.shrink = FALSE result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   is.shrink = FALSE),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, is.shrink = TRUE, shrink  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   is.shrink = TRUE, 
                                                   shrink = list(fun = 'var', 
                                                                 size = 3, 
                                                                 x = 0.5)),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: 
          params: ctr is controller, is.shrink = TRUE, shrink is Int result: error",
          {
            ctr <- theophylline()
            expect_error(pmx_plot_eta_cats(ctr = ctr, 
                                           is.shrink = TRUE, 
                                           shrink = 0))
          })

test_that("pmx_plot_eta_cats: 
           params: ctr is controller, is.shrink = TRUE, shrink is NULL  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   is.shrink = TRUE, 
                                                   shrink = NULL),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, covariates result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(
              ctr = ctr,
              covariates = pmx_cov(
                values = list("RACE", "DISE"),
                labels = list("Race", "Disease")
              )
            ),
            "gg"))
          })

test_that("pmx_plot_eta_cats: params: ctr is controller, labels result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_conts(
              ctr = ctr,
              labels = list(title = "EBE vs discrete covariates")
            ),
            "gg"))
          })

test_that("pmx_plot_eta_cats: params: no result: error",
          {
            expect_error(pmx_plot_eta_cats())
          })

test_that("pmx_plot_eta_cats: params: ctr is not controller result: error",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_eta_cats(ctr = ctr))
          })

test_that("pmx_plot_eta_cats: 
           params: ctr is controller, scale is sym  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   scale = "sym"),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: 
           params: ctr is controller, scale is sym, covariates  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   scale = "sym",
                                                   covariates=pmx_cov(values=list("SEX"),
                                                                      labels=list("SEX"))),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: 
           params: ctr is controller, scale is free  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   scale = "free"),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: 
           params: ctr is controller, scale is fix  result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_cats(ctr = ctr, 
                                                   scale = "fix"),
                                 "gg"))
          })

test_that("pmx_plot_eta_cats: 
          params: ctr is controller, scale is other result: error",
          {
            ctr <- theophylline() 
            expect_error(pmx_plot_eta_cats(ctr = ctr, 
                                           scale = "other"))
          })

#------------------- pmx_plot_eta_cats end ---------------------------------

#------------------- pmx_plot_eta_conts start ------------------------------
test_that("pmx_plot_eta_conts: params: ctr is controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_eta_conts(ctr = ctr), "gg"))
})

test_that("pmx_plot_eta_conts: params: ctr is controller, strat.color, strat.facet result: gg", {
  ctr <- theophylline()
  expect_true(inherits(
    pmx_plot_eta_conts(ctr = ctr, strat.color = "STUD", strat.facet =  ~ SEX),
    "gg"
  ))
})

test_that("pmx_plot_eta_conts: params: ctr is controller, covariates result: gg",
          {
            ctr <- theophylline()
            expect_true(inherits(pmx_plot_eta_conts(
              ctr = ctr,
              covariates = pmx_cov(
                values = list("WT0", "AGE0"),
                labels = list("Weight", "Age")
              )
            ),
            "gg"))
          })

test_that("pmx_plot_eta_conts: params: no result: error",
          {
            expect_error(pmx_plot_eta_conts())
          })

test_that("pmx_plot_eta_conts: params: ctr is not controller result: error",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_eta_conts(ctr = ctr))
          })
#------------------- pmx_plot_eta_conts end --------------------------------

#------------------- get_scale_y start -------------------------------------
test_that("get_scale_y: params: df is data set result: list", {
  mlxpath <- file.path(system.file(package = "ggPMX"),
                       "testdata",
                       "1_popPK_model",
                       "project.mlxtran")
  ctr <- pmx_mlxtran(mlxpath, config = "standing")
  dx <- ctr %>% get_data("eta")
  df <-
    unique(dx[, c("SEX", "RACE", "VALUE", "EFFECT"), with = FALSE])
  expect_true(inherits(get_scale_y(df), "list"))
})
#------------------- get_scale_y end ---------------------------------------

if (helper_skip()) {

  context("Test pmx-post_load")

  test_that("post_load: params:input, dxs are dataframes, dxs$finegrid is NULL;
          result: list", {
            ctr <- theophylline()
            dxs <- ctr$data
            input <- ctr$input
            dxs$sim_blq_npde_iwres <- data.frame(ID = c(1, 2, 3), TIME = c(0.0, 0.5, 1.0), T = c(0.1, 0.23, 1.2))
            dxs$sim_blq_y <- data.frame(ID = c(1, 2, 3), TIME = c(0.0, 0.5, 1.0), A = c(0.1, 0.23, 1.2))
            sys <- "mlx"
            dplot <- ctr$config$plots
            occ <- ""
            r <- post_load(dxs, input, sys, dplot, occ)
            expect_true(inherits(r, "list"))
          })

  test_that("post_load: params:input, dxs are dataframes, dxs$finegrid is NULL;
          result: list", {
            ctr <- theophylline()
            dxs <- ctr$data
            dxs$finegrid <- NULL
            input <- ctr$input
            sys <- "mlx"
            dplot <- ctr$config$plots
            occ <- ""
            r <- post_load(dxs, input, sys, dplot, occ)
            expect_true(inherits(r, "list"))
          })
  test_that("input_finegrid: params:input, finegrid;
          result: data.frame", {
            ctr <- theophylline()
            input <- ctr$input
            finegrid <- ctr$data$finegrid
            r <- input_finegrid(input, finegrid)
            expect_true(inherits(r, "data.frame"))
          })

  test_that("input_finegrid: params: input, finegrid is NULL;
          result: can read NONMEM-Output", {
            ctr <- theophylline()
            input <- ctr$input
            finegrid <- NULL
            r <- input_finegrid(input, finegrid)
            expect_equal(r, NULL)
          })

  test_that("post_load_eta: params:input, ds are dataframes, input$ID is factor, occ;
          result: error", {
            ctr <- theophylline()
            ds <- data.frame(ID = c(1, 2, 3), TIME = c(0.0, 0.5, 1.0), AMT = c(2000, 0, 0), Y = c(0, 130, 228))
            input <- data.frame(ID = c(4, 5, 6), EVID = c(1, 0, 0), AGE0 = c(73, 81, 69), DV = c(0, 130, 228))
            occ <- ctr$occ
            sys <- "mlx"
            expect_error(post_load_eta(ds, input, sys, occ))
          })

  test_that("post_load_eta: params:input, ds are dataframes, input$ID is factor, occ;
          result: error", {
            ctr <- theophylline()
            ds <- data.frame(ID = c(1, 2, 3), TIME = c(0.0, 0.5, 1.0), AMT = c(2000, 0, 0), Y = c(0, 130, 228))
            input <- data.frame(ID = c(1, 2, 3), EVID = c(1, 0, 0), AGE0 = c(73, 81, 69), DV = c(0, 130, 228))
            occ <- ctr$occ
            sys <- "mlx"
            r <- post_load_eta(ds, input, sys, occ)
            expect_true(inherits(r, "data.frame"))
          })

  test_that("post_load_eta: params:input, ds are dataframes, input$ID is factor, occ;
          result: error", {
            ctr <- theophylline()
            ds <- ctr$data$eta
            input <- ctr$input
            input$ID <- as.factor(input$ID)
            sys <- "mlx"
            occ <- "OCC"
            expect_error(post_load_eta(ds, input, sys, occ))
          })

  test_that("post_load_eta: params:input, ds are dataframes, ds$ID is factor, occ;
          result: error", {
            ctr <- theophylline()
            ds <- ctr$data$eta
            input <- ctr$input
            ds$ID <- as.factor(ds$ID)
            sys <- "mlx"
            occ <- "OCC"
            expect_error(post_load_eta(ds, input, sys, occ))
          })
}

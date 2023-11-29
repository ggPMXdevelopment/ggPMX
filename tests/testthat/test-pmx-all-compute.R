if (helper_skip()) {

  context("Test computing Shrinkage")
  ctr <- theophylline()
  ctr2 <- data.table(x = c(2, 3))

  #------------------- pmx_comp_shrink start ------------------------------------


  test_that("pmx_comp_shrink: params: ctr result: sd and var are calculated correctly", {
    expect_identical(
      c(0.1125175, 0.9469996, 0.7423478, 0.0579371, 0.7697818, 0.4924055),

      round(digits=7, as.vector(
        sapply(c("var", "sd"), function(n) pmx_comp_shrink(ctr, fun=n)[["SHRINK"]]))
        )

    )
  })


  test_that("pmx_comp_shrink: params: ctr is controller result:data.table", {
    expect_true(inherits(pmx_comp_shrink(ctr = ctr), "data.table"))
  })

  test_that("pmx_comp_shrink: params: ctr is controller
          result: identical structure", {
            expect_identical(
              colnames(pmx_comp_shrink(ctr = ctr)),
              c("EFFECT", "OMEGA", "SHRINK", "POS", "FUN")
            )
            expect_identical(
              pmx_comp_shrink(ctr = ctr)[[1]],
              c("Cl", "V", "ka")
            )
            comp_shr <- pmx_comp_shrink(ctr = ctr)
            c("Cl", "V", "ka")
            expect_true(is.null(comp_shr$strat.facet))
            expect_true(is.null(comp_shr$strat.color))
            expect_true(is.null(comp_shr$filter))
          })

  test_that("pmx_comp_shrink: params: ctr is controller, filter
          result: identical structure", {
            comp_shr <- pmx_comp_shrink(ctr = ctr, filter = FUN == sd)
            expect_true(is.null(comp_shr$filter))
          })

  test_that("pmx_comp_shrink: params: ctr is controller result:data.table", {
    expect_true(inherits(pmx_comp_shrink(ctr = ctr), "data.table"))
  })

  test_that(
    "pmx_comp_shrink: params:  ctr is controller and function in (var,sd)
          result: right structure of result data.table",
    {
      expect_true(all(sapply(pmx_comp_shrink(ctr = ctr, fun = "sd")[[5]], function(x) {
        x == "sd"
      })))
    }
  )

  test_that("pmx_comp_shrink: params: ctr is not controller result: error", {
    expect_error(pmx_comp_shrink(ctr = ctr2))
  })
  #------------------- pmx_comp_shrink end --------------------------------------
}

context("Test computing Shrinkage")
ctr <- theophylline()
ctr2 <- data.table(x = c(2, 3))

#------------------- pmx_comp_shrink start ------------------------------------
test_that("pmx_comp_shrink: params: ctr is controller
          result: identical structure",
          {
            expect_identical(colnames(pmx_comp_shrink(ctr = ctr)),
                             c("EFFECT", "OMEGA", "SHRINK", "POS", "FUN"))
            expect_identical(pmx_comp_shrink(ctr = ctr)[[1]],
                             c("Cl", "V", "ka"))
          })

test_that("pmx_comp_shrink: params: ctr is controller result:data.table",
          {
            expect_true(inherits(pmx_comp_shrink(ctr = ctr), "data.table"))
          })

test_that(
  "pmx_comp_shrink: params:  ctr is controller and function in (var,sd)
          result: right structure of result data.table",
  {
    expect_true(all(sapply(pmx_comp_shrink(ctr = ctr, fun = "sd")[[5]], function(x)
      x == "sd")))
    
  }
)

test_that("pmx_comp_shrink: params: ctr is not controller result: error", {
  expect_error(pmx_comp_shrink(ctr = ctr2))
})
#------------------- pmx_comp_shrink end --------------------------------------

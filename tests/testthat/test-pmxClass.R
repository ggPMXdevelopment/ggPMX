context("Test pmxClass")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("can create pmx class", {
  ctr <- pmx_mlx("standing")
  expect_is(ctr, "pmxClass")
})

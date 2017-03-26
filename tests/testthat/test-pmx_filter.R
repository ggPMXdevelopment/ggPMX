context("Test filtering of controller data")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("canFilterData", {
  ctr <- pmx_mlx("standing")
  expect_is(ctr, "pmxClass")
  oldData <- ctr$data
  out <- ctr %>% pmx_filter(data_set = "ind_pred", ID <= 5)
  expect_gt(dim(oldData$ind_pred[ID > 5])[1], 0L)
  expect_equal(dim(out$data$ind_pred[ID > 5])[1], 0L)
})


if (helper_skip()) {

  context("Test filtering of controller data")
  pmxClassHelpers <- test_pmxClass_helpers()

  test_that("canFilterData", {
    ctr <- pmxClassHelpers$ctr
    expect_is(ctr, "pmxClass")
    oldData <- ctr$data
    out <- ctr %>% pmx_filter(data_set = "eta", ID <= 5)
    expect_gt(dim(oldData$eta[ID > 5])[1], 0L)
    expect_equal(dim(out$data$eta[ID > 5])[1], 0L)
  })
}

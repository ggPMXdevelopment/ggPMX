context("Test shrinkage computation")
pmxClassHelpers <- test_pmxClass_helpers()


test_that("test shrinkage for standing config", {
  
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  est_ <- ctr %>% get_data("estimates")
  eta_ <- ctr %>% get_data("eta")
  res <- shrinkage(est_,eta_)
  expect_is(res,"data.frame")
  expect_equal(colnames(res), c("EFFECT", "VALUE_OMEGA","SHRINK"))
  expect_true(all(res$SHRNK < 1))

})


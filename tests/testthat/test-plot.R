context("Test filtering of controller data")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("individual plot: get all pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv")
  expect_equal(length(p),6)
  
})


test_that("individual plot: get single page", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",2)
  expect_equal(length(p),1)
  
})



test_that("individual plot: get some pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",c(2,4))
  expect_equal(length(p),2)
  
})

test_that("individual plot : don't exced the effective number of pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",1:100)
  expect_equal(length(p),6)
  
})

context("Test filtering of controller data")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("individual plot: get all pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv")
  expect_equal(length(p),13)
  
})


test_that("individual plot: get single page", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",2)
  expect_true(inherits(p,"ggplot"))
  
})



test_that("individual plot: get some pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",c(2,4))
  expect_equal(length(p),2)
  
})

test_that("individual plot : don't exceed the effective number of pages", {
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",1:100)
  expect_equal(length(p),13)
  
})

test_that("can create a plot using setting dname", {
  
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  ctr %>% set_plot("DIS", pname = "distr1", type = "box",dname="eta")
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  expect_equal(length(pconf$plot$layers), 2)
})


test_that("Create a plot with not valid dname throw error", {
  
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  expect_message(
    ctr %>% set_plot("DIS", pname = "distr1", type = "box",dname="xxx")
  )
  
})


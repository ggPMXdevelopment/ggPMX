context("Test filtering of controller data")

test_that("individual plot: get all pages", {
  ctr <- pmx_mlx("standing")
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv")
  expect_equal(length(p),5)
  
})


test_that("individual plot: get single page", {
  ctr <- pmx_mlx("standing")
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",2)
  expect_equal(length(p),1)
  
})



test_that("individual plot: get some pages", {
  ctr <- pmx_mlx("standing")
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",c(2,4))
  expect_equal(length(p),2)
  
})

test_that("individual plot : don't exced the effective number of pages", {
  ctr <- pmx_mlx("standing")
  expect_is(ctr, "pmxClass")
  p <- ctr %>% get_plot("indiv",1:100)
  expect_equal(length(p),5)
  
})

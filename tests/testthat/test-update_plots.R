context("Test update plots")
helpers <- helper_updateplots()

test_that("can update DIS plot", {
  ctr <- helpers$ctr
  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
  expect_true("distr1" %in% ctr$plots())
  p <- ctr %>% get_plot("distr1")
})

test_that("can remove DIS plot", {
  ctr <- helpers$ctr
  ctr$remove_plot("distr1")
  expect_false("distr1" %in% ctr$plots())
})

context("Test update plots")
helpers <- helper_updateplots()

test_that("can update DIS plot", {
  ctr <- helpers$ctr
  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
  expect_true("distr1" %in% ctr$plots())
  p <- ctr %>% get_plot("distr1")
  oldconf <- ctr$get_config("distr1")
  expect_false(oldconf$has.shrink)
 
})

test_that("can remove DIS plot", {
  ctr <- helpers$ctr
  ctr$remove_plot("distr1")
  expect_false("distr1" %in% ctr$plots())
})

test_that("can update IND plot", {
  ctr <- helpers$ctr
  ctr %>% set_plot("IND", pname = "indiv1")
  expect_is(ctr %>% get_plot("indiv1", c(2, 4)),"list")
  expect_true("indiv1" %in% ctr$plots())
  oldconf <- ctr$get_config("indiv1")
  expect_false(oldconf$gp$has.band)
  
  ctr %>% pmx_update("indiv1", has.band = TRUE)
  newconf <- ctr$get_config("indiv1")
  expect_true(newconf$gp$has.band)
})

test_that("can remove IND plot", {
  ctr <- helpers$ctr
  ctr$remove_plot("indiv1")
  expect_false("indiv1" %in% ctr$plots())
})

test_that("can update RES plot", {
  
})

test_that("can remove RES plot", {
})

test_that("can update with filter", {
  # set new plot
  ctr <- helpers$ctr
  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
  ctr %>% get_plot("distr1")
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  expect_equal(length(pconf$data), 2)
  
  # Update plot with filter
  ctr %>% pmx_update("distr1", filter = ID < 10)
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  expect_equal(length(pconf$data), 2)
  
  # test can remove filter
  ctr %>% pmx_update("distr1", filter = NULL)
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  
  expect_equal(length(pconf$data),2)
})


test_that("can update facet stratification", {
  # set new plot
  ctr <- helpers$ctr
  p <- ctr %>% 
    pmx_update("ebe_hist",strat.facet ="SEX") %>%
    get_plot("ebe_hist")
  pconf <- ggplot2::ggplot_build(p)
  expect_true("SEX" %in% names(pconf$layout$panel_layout))
  
  })


test_that("can update indivual plot labels",{
  
  
  ctr <- helpers$ctr
  
  # Change x- and y-labels
  p2 <- ctr %>%
    pmx_update(
      "indiv",
      labels=list(x="Time (days)", y="Free serum concentration (nmol)")) %>%
    get_plot("indiv",npage=1)
  expect_identical(
  list(x=p2$labels$x,
       y=p2$labels$y),
  list(x="Time (days)", y="Free serum concentration (nmol)"))
  
})

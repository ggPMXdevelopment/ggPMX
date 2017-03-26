context("Test update plots")
helpers <- helper_updateplots()

test_that("can update DIS plot", {
  ctr <- helpers$ctr
  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
  expect_true("distr1" %in% ctr$plots())
  p <- ctr %>% get_plot("distr1")
  oldconf <- ctr$get_config("distr1")
  expect_false(oldconf$has.shrink)
  shrink <- data.frame(EFFECT = c("cl", "ka", "v"), 
                       SHRINK = c(0.1, 0.2, 0.7), 
                       stringsAsFactors = FALSE)
  ctr %>% pmx_update(
    "distr1",
    has.shrink = TRUE,
    shrink = shrink
  )
  newconf <- ctr$get_config("distr1")
  expect_identical(newconf$gp$shrink, shrink)
  
})

test_that("can remove DIS plot", {
  ctr <- helpers$ctr
  ctr$remove_plot("distr1")
  expect_false("distr1" %in% ctr$plots())
})

test_that("can update IND plot", {
  ctr <- helpers$ctr
  ctr %>% set_plot("IND", pname = "indiv1")
  expect_silent(ctr %>% get_plot("indiv1", c(2,4)))
  expect_true("indiv1" %in% ctr$plots())
  oldconf <- ctr$get_config("indiv1")
  expect_true(oldconf$gp$has.band)
  
  ctr %>% pmx_update("indiv1", has.band = FALSE)
  newconf <- ctr$get_config("indiv1")
  expect_false(newconf$gp$has.band)
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

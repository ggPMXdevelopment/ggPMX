context("Test update plots")
helpers <- helper_updateplots()

test_that("can update DIS plot", {
  ctr <- helpers$ctr
  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
  expect_true("distr1" %in% ctr$plots())
  p <- ctr %>% get_plot("distr1")
  oldconf <- ctr$get_config("distr1")
  expect_true(oldconf$has.shrink)
  shrink <- data.frame(EFFECT = c("cl", "ka", "v"), 
                       SHRINK = c(0.1, 0.2, 0.7), 
                       stringsAsFactors = FALSE)
  ctr %>% update_plot(
    "distr1",
    has.shrink = TRUE,
    shrink = shrink
  )
  newconf <- ctr$get_config("distr1")
  expect_identical(newconf$shrink, as.list(shrink))
  
})

test_that("can remove DIS plot", {
  ctr <- helpers$ctr
  ctr$remove_plot("distr1")
  expect_false("distr1" %in% ctr$plots())
})

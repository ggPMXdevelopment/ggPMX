context("Test graphical parameters object")

test_that("pmx_gpar defaults are well setted", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_identical(
    gpars$draft, 
    list(size = 20, label = "DRAFT", color = "grey50")
  )
  expect_identical(
    gpars$smooth, 
    list(se = FALSE, linetype = 2, size = 1.5, method = "loess")
  )
  
})

test_that("can test object is pmx_gpar", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_true(is.pmx_gpar(gpars))
})

test_that("can print pmx_gpar object", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_output(print(gpars), "\"hello\"")
})
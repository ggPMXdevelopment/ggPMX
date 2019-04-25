context("Test graphical parameters object")

test_that("pmx_gpar defaults are well setted", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_identical(
    gpars$draft,
    list(size = 5, label = "DRAFT", colour = "grey50", x = Inf, y = -Inf)
  )
  expect_identical(
    gpars$smooth,
    list(
      se = FALSE, linetype = 1, size = 1.5, method = "loess",
      colour = "red"
    )
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

context("Test config")

test_that("can source configs", {
  conf <- configs()
  expect_identical(conf$name, c("config1", "standing"))
})

test_that("can print configs", {
  conf <- configs()
  expect_output(print(conf), "There are 2 configs for mlx system ")
})

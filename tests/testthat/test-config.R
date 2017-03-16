context("Test config")

test_that("can source configs", {
  conf <- configs()
  expect_identical(conf$name, c("config1", "standing"))
})

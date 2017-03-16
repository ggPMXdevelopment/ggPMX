context("Test pmx options")

test_that("can get pmx options", {
  pmxOptions(work_dir = "/home/agstudy")
  default_options <- pmxOptions()
  expect_identical(default_options$work_dir, "/home/agstudy")
})

test_that("can set option", {
  pmxOptions(myOption = 10L)
  expect_identical(getPmxOption("myOption"), 10L)
})

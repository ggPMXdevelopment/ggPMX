library(purrr)
context("Test pmx options")

test_that("can get pmx options", {
  pmxOptions(template_dir = "/home/agstudy")
  default_options <- pmxOptions()
  expect_identical(default_options$template_dir, "/home/agstudy")
})


test_that("getPmxOption params result: NULL", {
  expect_true(is.null(getPmxOption("myOption")))
})


test_that("can set option", {
  pmxOptions(myOption = 10L)
  expect_identical(getPmxOption("myOption"), 10L)
})


test_that("can set option", {
  pmxOptions(myOption = 1L)
  expect_identical(getPmxOption("myOption"), 1L)
})

test_that("getPmxOption params result: options name", {
  get_opt <- getPmxOption("myOpt")
  expect_true(is.null(get_opt$name))
})


test_that("getPmxOption params result: options name must be a string", {
  expect_error(getPmxOption(myOption))
})


test_that("getPmxOption params: NULL", {
  expect_error(getPmxOption())
})


test_that("pmxOptions result: identical names", {
  expect_true(all(c("template_dir", "myOption") %in% names(pmxOptions())))
})


test_that("checkPmxOption params: value, pmxname, default result: identical name", {
  pmxOptions(myOption = 10L)
  expect_identical(checkPmxOption("myOption"), "myOption")
})


test_that("pmxOptions result: identical inherits", {
  expect_true(inherits(pmxOptions(), "list"))
})


test_that("pmxOptions params: template_dir result:  identical name", {
  pmxO <- pmxOptions(template_dir = "myOption")
  expect_equal(names(pmxO), "template_dir")
})


test_that("pmxOptions result: identical inherits", {
  pmxOptions(myOption = 10L)
  expect_true(inherits(getPmxOption("myOption"), "integer"))
})

test_that("pmxOptions params:NULL result: error ", {
  expect_error(pmxOptions(NULL))
})


test_that("checkPmxOption params: NULL result: error missing arguments", {
  expect_error(checkPmxOption())
})

test_that("checkPmxOption params:pmxname, value = NULL result: error set a NULL argument or
          global myOption option", {
  expect_error(checkPmxOption(value = NULL, "myOption"))
})


test_that("checkPmxOption params: NULL result: identical inherits", {
  expect_true(inherits(checkPmxOption("myOption"), "character"))
})


test_that("checkPmxOption params: pmxname, default result: identical inherits", {
  default_options <- pmxOptions(template_dir = "/home/agstudy")
  expect_true(inherits(checkPmxOption("myOption", default = default_options), "character"))
})

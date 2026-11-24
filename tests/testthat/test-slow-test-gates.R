test_that("core tests do not depend on NOT_CRAN", {
  withr::local_envvar(NOT_CRAN = "false", GGPMX_RUN_SLOW_TESTS = NA)
  expect_identical(helper_skip(), TRUE)
  expect_identical(helper_run_slow_tests(), FALSE)
  withr::local_envvar(NOT_CRAN = "true")
  expect_identical(helper_skip(), TRUE)
  expect_identical(helper_run_slow_tests(), FALSE)
})

test_that("extended tests require explicit opt-in", {
  for (value in c("", "false", "1", "invalid")) {
    withr::local_envvar(GGPMX_RUN_SLOW_TESTS = value)
    expect_identical(helper_run_slow_tests(), FALSE)
  }
  for (value in c("true", "TRUE")) {
    withr::local_envvar(GGPMX_RUN_SLOW_TESTS = value)
    expect_identical(helper_run_slow_tests(), TRUE)
  }
})

test_that("Windows integrations remain active elsewhere and in extended CI", {
  withr::local_envvar(GGPMX_RUN_SLOW_TESTS = "false")
  expect_identical(helper_run_windows_integration("windows"), FALSE)
  expect_identical(helper_run_windows_integration("unix"), TRUE)
  withr::local_envvar(GGPMX_RUN_SLOW_TESTS = "true")
  expect_identical(helper_run_windows_integration("windows"), TRUE)
})

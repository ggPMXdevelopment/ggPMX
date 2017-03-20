context("Test tools")

test_that("Can call plot tester", {
  with_mock(
    `shiny::runApp` = function(...){"hello"},
    res <- plotTester()
  )
  expect_identical(res, "hello")
})

test_that("plot tester error functions", {
  with_mock(
    `base::system.file` = function(...){""},
    expect_error(plotTester(), "Could not find example directory")
  )
})

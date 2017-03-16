context("Test tools")

test_that("Can call plot tester", {
  with_mock(
    `shiny::runApp` = function(...){"hello"},
    res <- plotTester()
  )
  expect_identical(res, "hello")
})

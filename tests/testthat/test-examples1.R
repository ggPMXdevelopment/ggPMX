# Unit tests example, part 1
# --------------------------
#
# DELETE THIS FILE FROM YOUR PACKAGE IF THE EXAMPLE IS NO LONGER NEEDED.
#
# The following is an example how the package's functionality should
# be unit tested. Each test file represents a context which should map
# to one of the major steps as outlined in the vignette of this
# vignette. As you will most likely have more tests than would fit in
# this logic, also add a test-misc.R file. DO NEVER CHANGE THE ORDER
# OF THE TESTS, IF POSSIBLE. The order must stay constant for
# future reference. Should you add new test files, these need to be
# registered in the inst/extra/run-oq.R file to ensure execution in
# the right order for OQ testing.
#
# Make sure to write tests for all exported functions. Always provide
# "positive" tests, i.e. testing for correctness and also consider to
# provide "negative" tests which test for thrown error messages
# whenever wrong input is provided to a function, for example. We
# strongly recommend to execute the examples of each function and then
# test for the correctness of the results produced in that example
# (below is an example for the subtract function).
#
# Have a look at the comments below for some tips & tricks.

context("Arithmetic operations: Example functions")

testAdd <- function() {
  
  # Example how to make use of the examples in the object documentation
  # (-> avoid duplication of code in the examples section and tests):
  
  # Run the examples from add (will set the variables add1 and add2)
  suppressMessages(example("add", package="ggPMX",
                           echo=FALSE, ask=FALSE, verbose=FALSE))
  expect_equal(add1, 2)
  expect_equal(add2, 11)
  
  # We recommend to run all examples as part of the tests, however, extra tests 
  # should be added as needed that are not reflected in the examples)
  
  expect_equal(add(0, -1), -1)
  
}


testAddException <- function() {
    expect_error(add(1, "2"))
    expect_error(add(factor(1), 2))
}


testSubtract <- function() {
  
  # Run the example from subtract
  suppressMessages(example("subtract", package="ggPMX",
                           echo=FALSE, ask=FALSE, verbose=FALSE))
  expect_equal(subtract1, 0)
  expect_equal(subtract2, 11)
  
}


testSubtractException <- function() {
  expect_error(subtract(1, "2"))
  expect_error(subtract(1, factor(2)))
}


test_that("Addition works correctly for numeric input", testAdd())
test_that("Addition works correctly for non-numeric input", testAddException())
test_that("Subtraction works correctly for numeric input", testSubtract())
test_that("Subtraction works correctly for non-numeric input",
          testSubtractException())

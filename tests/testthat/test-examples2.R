# Unit tests example, part 2
# --------------------------
#
# DELETE THIS FILE FROM YOUR PACKAGE IF THE EXAMPLE IS NO LONGER NEEDED.

# See test-examples.R for more details on how units tests should be structured.

context("Data retrieval example function.")


testRetrieveData <- function() {
  
  suppressMessages(example("retrieve_data", package="ggPMX",
                           echo=FALSE, ask=FALSE, verbose=FALSE))
  expect_equal(dat21, 4)
  expect_equal(dat52, 50)
  
  expect_error(retrieve_data(8))
}


test_that("Data retrieval", testRetrieveData())

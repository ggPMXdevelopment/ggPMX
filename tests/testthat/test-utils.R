context("Test utility functions")

test_that("merge vectors error works", {
  expect_error(
    mergeVectors.(1:4, 5:8),
    "Vectors must be either NULL or have names for all elements"
  )
})

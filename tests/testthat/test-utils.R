context("Test utility functions")

test_that("merge vectors error works", {
  expect_error(
    mergeVectors.(1:4, 5:8),
    "Vectors must be either NULL or have names for all elements"
  )
})

test_that("l_left_join merge compound lists", {
  res <-
    l_left_join(
      list(
        x = 1,
        y = 1,
        h = list(z = 1)
      ),
      list(
        y = 2,
        h = list(h = 4)
      )
    )
  expected <- list(x = 1, h = list(z = 1, h = 4), y = 2)
  expect_identical(res, expected)
})

test_that("pk_pd is worrking", {
  ctr <- pk_pd()
  expect_s3_class(ctr, "pmxClass")
})

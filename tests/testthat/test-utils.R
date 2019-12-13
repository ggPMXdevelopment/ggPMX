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

  res2 <-
    l_left_join(
      list(),
      list(
        x = 1,
        y = 1,
        h = list(z = 1)
      )
    )
  expected2 <- list(x = 1, y = 1, h = list(z = 1))
  expect_identical(res2, expected2)
})

test_that("pk_pd is worrking", {
  ctr <- pk_pd()
  expect_s3_class(ctr, "pmxClass")
})

test_that("theophylline is working", {
  ctr <- theophylline()
  expect_s3_class(ctr, "pmxClass")
})

test_that("pk_occ is working", {
  ctr <- pk_occ()
  expect_s3_class(ctr, "pmxClass")
})

test_that("abbrev provides correct abbreviation expansions", {
  expect_identical(abbrev("AIC"), "Akaike information criterion")
  expect_identical(abbrev("DV"), "Dependent variable")
  expect_identical(abbrev("ETA"), "Inter-individual random effect")
  expect_identical(abbrev("FOCEI"), "First order conditional estimation with interaction")
  expect_identical(abbrev("SAEM"), "Stochastic approximation of the expectation-minimization algorithm")
  expect_identical(abbrev("TIME"), "Time after first dose (hours)")
  expect_identical(abbrev("|IWRES|"), "|Individual weighted residuals|")
  expect_identical(abbrev("VPC"), "Visual predictive check")

  expect_identical(typeof(abbrev()), "list")
  expect_identical(abbrev("FAKE"), "FAKE")
})

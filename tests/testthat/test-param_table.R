context("Test param_table()")

test_that("can make param_table()", {

  # Creating "pmxClass" controllers:
  ctr <- theophylline()

  ctr_nm <- pmx_nm(
    directory=file.path(system.file(package = "ggPMX"), "testdata","extdata"),
    runno="001"
  )

  # Creating kable outputs for testing:
  p_ctr <- param_table(ctr, digits=2, scientific=FALSE)
  p_ctr_nm <- param_table(ctr_nm, digits=2, scientific=FALSE)
  p_ctr_sci <- param_table(ctr, digits=2, scientific=TRUE)

  # Check headers
  expect_true(
    "|Parameter      |  Value| Relative Standard Error|      Shrinkage|" %in% trimws(p_ctr)
  )

  expect_true(
    "|Parameter  |  Value| Relative Standard Error|     Shrinkage|" %in% trimws(p_ctr_nm)
  )

  #check a random row (here 5) of param_table
  expect_true(
    "|Cl_pop         |   0.31|                      8%|               |" %in% trimws(p_ctr),
  )

  expect_true(
    "|THETA1     |     26|                      3%|              |" %in% trimws(p_ctr_nm),
  )

  # Check class:
  expect_s3_class(p_ctr, "knitr_kable")
  expect_s3_class(p_ctr_nm, "knitr_kable")
  # Check output lengths:
  expect_length(p_ctr, 23L)
  expect_length(p_ctr_nm, 23L)
  expect_length(p_ctr_sci, 23L)

  # Check scientific notation:
  p_ctr_sci <- param_table(ctr, digits=2, scientific=TRUE)
  expect_true(any(grepl("\\de\\+\\d", p_ctr_sci)))
  expect_true(any(grepl("\\de\\-\\d", p_ctr_sci)))
})

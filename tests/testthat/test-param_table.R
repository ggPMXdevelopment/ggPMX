context("Test param_table()")

test_that("can make param_table()", {
  
  ctr <- theophylline()
  
  nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata","extdata")
  ctr_nm <- pmx_nm(directory = nonmem_dir, runno = "001")
  
  p_ctr <- param_table(ctr)
  
  p_ctr_nm <- param_table(ctr_nm)
  
  #check headers of param_table
  expect_identical(
    trimws(p_ctr[[1]]),"|Parameter      |  Value| Relative Standard Error|      Shrinkage|"
  )
  
  expect_identical(
    trimws(p_ctr_nm[[1]]),"|Parameter  |  Value| Relative Standard Error|     Shrinkage|"
  )
  
  #check a random row (here 5) of param_table
  expect_identical(
    trimws(p_ctr[[5]]), "|Cl_pop         |   0.31|                      8%|               |"
  )

  expect_identical(
    trimws(p_ctr_nm[[5]]),  "|THETA1     |     26|                      3%|              |"
  )
  
})

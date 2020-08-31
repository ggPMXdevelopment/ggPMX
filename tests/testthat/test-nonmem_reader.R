context("nonmem_reader")

test_that("can read NONMEM-Output", {
  
  ctr <- NULL
  nonmem_dir <- "/home/hamzise1/svn_local/USER_EXAMPLES/NM/2.PK_WITH_COVARIATE/PsN_NM73"
  ctr <- pmx_nm(directory = nonmem_dir, runno = 1)
  
  vec <- c(ctr$cats, ctr$conts)
  
  #Check if ctr is made
  expect_identical(
    is.null(ctr),FALSE 
  )
  
  #Check if header is correctly extracted and named from input
  expect_identical(
    names(ctr$input), c("ID","TIME","IPRED","IWRES","NPDE","EVID","MDV","DV","PRED","RES","WRES","ALAG1","KA","V","CL","ETA1","ETA2","ETA3","ETA4","SEX","WT")
  )
  
  #Check if covariates were extracted correctly
  expect_identical(
    vec,c("SEX","WT") 
  )
  
})

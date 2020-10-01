context("Test pmx_nm")

test_that("can read NONMEM-Output", {
  
  ctr <- NULL
  nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata","extdata")
  
  ctr <- pmx_nm(directory = nonmem_dir, runno = "001")
  
  #Check if ctr is made
  expect_identical(
    is.null(ctr),FALSE 
  )
  
  #Check if header is correctly extracted and named from input
  expect_identical(
    names(ctr$input), c("ID","SEX","MED1","MED2","DOSE","AMT","SS","II","TIME","TAD","IPRED",
                        "CWRES","CPRED","IWRES","EVID","A1","A2","DV","PRED","RES","WRES","CLCR",
                        "AGE","WT","KA","CL","V","ALAG1","ETA1","ETA2","ETA3")
  )
  
  #Check if covariates were extracted correctly
  vec <- c(ctr$cats, ctr$conts)
  
  expect_identical(
    vec,c("SEX","MED1","MED2","CLCR","AGE","WT") 
  )
  
  #Check if data was extracted correctly
  csv_dir <- file.path(system.file(package = "ggPMX"), "testdata","extdata","for_testing")
  
  input_file <- file.path(csv_dir, "predictions.csv")
  dat <- read.csv(input_file)
  
  expect_equal(
    nrow(ctr %>% get_data("predictions")), nrow(dat)
    )
  
  #Check if simulation data was extracted correctly
  simput_file <- file.path(csv_dir, "sim.csv")
  sim_dat <- read.csv(simput_file)
  
  expect_equal(
    nrow(ctr %>% get_data("sim")), nrow(sim_dat)
    )
  
})

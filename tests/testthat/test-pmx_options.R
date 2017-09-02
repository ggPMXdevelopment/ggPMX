context("Test pmx options")

test_that("can get pmx options", {
  pmxOptions(template_dir = "/home/agstudy")
  default_options <- pmxOptions()
  expect_identical(default_options$template_dir, "/home/agstudy")
})

test_that("can set option", {
  pmxOptions(myOption = 10L)
  expect_identical(getPmxOption("myOption"), 10L)
})


test_that("Initiating controlers work with and without specification of covariates",{
  
  
  theophylline <- file.path(system.file(package = "ggPMX"), "testdata", 
                            "theophylline")
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  
  ctr <- pmx_mlx(
    "standing", 
    directory =  WORK_DIR, 
    input = input_file, 
    dv = "Y",
    dvid="DVID"
  )
  expect_equal(ctr %>% get_covariates(),"")
  uc.name <-  "1_popPK_model"
  data_file <-  "PKdata_ggPMX.csv"
  
  uc.dir <- file.path(system.file(package = "ggPMX"), "testdata", 
                      uc.name)
  wd.mlx <- file.path(uc.dir, "Monolix")
  input_file <- file.path(uc.dir, data_file)
  
  ctr <- pmx_mlx("standing",
    directory = wd.mlx, 
    input = input_file, 
    dv = "DV", 
    dvid = "ytype", 
    occ = "ISS",
    cats = c("SEX","RACE","DISE","ILOW"), 
    conts = c("AGE0","WT0","HT0","TRT")
  )
  
  expect_equal(ctr %>% get_covariates(),
               c("SEX", "RACE", "DISE", "ILOW", "AGE0", "WT0", "HT0", "TRT"))
  
  
})
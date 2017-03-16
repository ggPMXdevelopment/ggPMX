context("Test reader parameters")
reader_help <- reader_helpers()

test_that("load standing config", {
  expect_silent(load_config("standing", "mlx"))
  expect_identical(reader_help$conf$sys, "mlx")
})


test_that("can load data set", {
  names. <- names(reader_help$conf$data)
  datasets <- reader_help$conf$data[names.]
  path <- reader_help$wd
  sys <- reader_help$conf$sys
  dxs <- lapply(datasets,
                load_data_set,
                path = path,
                sys = sys)
  expect_identical(names(dxs$par_est), 
                   c("PARAM", "VALUE", "SE", "RSE", "PVALUE"))
  expect_identical(names(dxs$mod_pred), 
                   c("ID", "TIME", "DV", "PRED", "NPDE", "IPRED", "IWRES"))
  expect_identical(names(dxs$ind_pred), 
                   c("ID", "EVID", "TWT", "TAGE", "SEX", "STUD", 
                     "VARIABLE", "VALUE", 
                     "VAR", "EFFECT", "FUN"))
})

test_that("errors work in load data set", {
  names. <- names(reader_help$conf$data)
  datasets <- reader_help$conf$data[names.]
  path <- reader_help$wd
  sys <- reader_help$conf$sys
  datasets$par_est$file <- c("\\.txt")
  with_mock(
    `base::grep` = function(pattern, ...){
      if(identical(pattern, "mlx")){
        return("nonexistant file")
      }else{
        return(list.files(path, full.names = TRUE))
      }
    }, 
    expect_error(
      load_data_set(datasets$par_est, path, sys), "FILE DOES NOT exist"
    )
  )
  
})

test_that("load data set functions when reader does not exist", {
  names. <- names(reader_help$conf$data)
  datasets <- reader_help$conf$data[names.]
  path <- reader_help$wd
  sys <- reader_help$conf$sys
  names(datasets$mod_pred$names) <- c("id", "time", "y1", "poppred", 
                                      "npde")
  with_mock(
    `data.table::fread` = function(...){
      data.table::setDT(
        data.frame(id = 1, time = 2, y1 = 3, poppred = 4, npde = 5,
                   V3 = 6)
      )
    },
    `base::exists` = function(...){FALSE},
    res <- load_data_set(datasets$mod_pred, path, sys)
  )
  expect_identical(names(res), c("ID", "TIME", "DV", "PRED", "NPDE"))
})

test_that("can exclude data set", {
  exclude <- load_source(sys = reader_help$conf$sys, path = reader_help$wd, 
                         dconf = reader_help$conf$data, 
                         exclude = "ind_pred")
  expect_identical(names(exclude), c("par_est", "mod_pred"))
})

test_that("can include data set", {
  include <- load_source(sys = reader_help$conf$sys, path = reader_help$wd, 
                         dconf = reader_help$conf$data, 
                         include = "ind_pred")
  expect_identical(names(include), "ind_pred")
})

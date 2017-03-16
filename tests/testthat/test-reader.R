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

test_that("can read mlx ind", {
  
})


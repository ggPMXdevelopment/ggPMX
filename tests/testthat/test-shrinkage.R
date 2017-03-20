context("Test shrinkage computation")
reader_help <- reader_helpers()

test_that("test shrinkage for standing config", {
  names. <- names(reader_help$conf$data)
  datasets <- reader_help$conf$data[names.]
  path <- reader_help$wd
  sys <- reader_help$conf$sys
  dxs <- lapply(datasets,
                load_data_set,
                path = path,
                sys = sys)
  
  expect_silent(shrinkage(dxs[["par_est"]],dxs[["ind_pred"]]))
  
  
})


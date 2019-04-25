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
  dxs <- lapply(
    datasets,
    load_data_set,
    path = path,
    sys = sys
  )
  expect_identical(
    names(dxs[["estimates"]]),
    c("PARAM", "VALUE", "SE", "RSE", "PVALUE")
  )
  expect_identical(
    names(dxs[["predictions"]]),
    c("ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES")
  )

  expect_identical(names(dxs[["finegrid"]]), c("ID", "TIME", "PRED", "IPRED"))
})

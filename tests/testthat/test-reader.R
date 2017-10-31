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
    c("ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES", "DVID")
  )

  expect_identical(names(dxs[["finegrid"]]), c("ID", "TIME", "PRED", "IPRED", "DVID"))
})

test_that("errors work in load data set", {
  names. <- names(reader_help$conf$data)
  datasets <- reader_help$conf$data[names.]
  path <- reader_help$wd
  sys <- reader_help$conf$sys
  datasets$estimates$file <- c("\\.txt")
  with_mock(
    `base::grep` = function(pattern, ...) {
      if (identical(pattern, "mlx")) {
        return("nonexistant file")
      } else {
        return(list.files(path, full.names = TRUE))
      }
    },
    expect_message(
      load_data_set(datasets$estimates, path, sys), "FILE DOES NOT exist"
    )
  )
})

context("Test reader parameters")
reader_help <- reader_helpers()

test_that("load standing config", {
  expect_silent(load_config("standing", "mlx"))
  expect_identical(reader_help$conf$sys, "mlx")
})

test_that("check that datasets with double ID columns are loaded", {
  pkpd_path <- file.path(system.file(package = "ggPMX"), "testdata")
  pkpd_id_path <- file.path(tempdir(), "pk_pd_id")
  dir.create(pkpd_id_path)
  invisible(file.copy(list.files(file.path(pkpd_path, "pk_pd"), full.names = TRUE), pkpd_id_path,
    recursive = TRUE
  ))
  pred_list <- list("predictions1.txt", "predictions2.txt")
  for (pred_file in pred_list) {
    f <- read.delim(file.path(pkpd_id_path, "RESULTS", pred_file),
      header = TRUE, sep = "\t",
      dec = "."
    )
    f$ID_2 <- f$ID
    write.table(f, file.path(pkpd_id_path, "RESULTS", pred_file),
      sep = "\t", dec = ".",
      row.names = FALSE
    )
  }
  pkpd_work_dir <- file.path(pkpd_id_path, "RESULTS")
  pkpd_input_file <- file.path(pkpd_id_path, "pk_pd.csv")

  ep <- pmx_endpoint(
    code = "4", label = "some_label", unit = "some_unit", file.code = "2",
    trans = "log10"
  )

  expect_no_error(pmx_mlx(
    directory = pkpd_work_dir, input = pkpd_input_file, dv = "dv",
    dvid = "dvid", endpoint = ep
  ))
  invisible(unlink(pkpd_id_path, recursive = TRUE))
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

if (helper_skip()) {

  context("Test reader parameters")
  reader_help <- reader_helpers()

  #------------------- read_mlx18_res start --------------------------------------
  test_that("read_mlx18_res: params: path, x; result: data.table", {
    skip_on_os("windows")
    ipath <- file.path(reader_help$wd, "predictions.txt")
    names. <- names(reader_help$conf$data)
    x <- reader_help$conf$data[names.]
    expect_error(read_mlx18_res(ipath, x[["predictions"]]))
  })

  test_that("read_mlx18_res: params: path is wrong, subfolder is not empty,
          file exists, endpoint; result: identical structure", {
            skip_on_os("windows")
            ipath <- file.path(reader_help$wd, "predictions.txt")
            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$predictions$endpoint <- "pred"
            expect_error(read_mlx18_res(ipath, x$predictions))
          })

  test_that("read_mlx18_res: params: path is wrong, subfolder is not empty,
          file exists; result: identical structure", {
            skip_on_os("windows")
            ipath <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline", "predictions.txt")

            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$predictions$subfolder <- "Monolix"
            x$predictions$file <- "predictions.txt"
            expect_null(read_mlx18_res(ipath, x$predictions))
          })

  test_that("read_mlx18_res: params: newnames, path is wrong, subfolder is not empty,
          file exists; result: identical structure", {
            skip_on_os("windows")
            ipath <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline", "predictions.txt")

            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$predictions$subfolder <- "Monolix"
            x$predictions$file <- "predictions.txt"
            x$predictions$newnames <- list(
              ID = "id", TIME = "time", PRED = "poppred",
              NPDE = "npde", IPRED = "mlx_ipred", IWRES = "mlx_iwres"
            )

            r <- read_mlx18_res(ipath, x$predictions)
            expect_identical(r, NULL)
          })

  test_that("read_mlx18_res: params: pattern, path is wrong, subfolder is not empty,
          file exists; result: identical structure", {
            skip_on_os("windows")
            ipath <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline", "finegrid.txt")

            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$finegrid$names <- list(
              ID = "ID", time = "time", popPred = "popPred",
              "indPred_mean*" = "indPred_mean*", indPred_mode = "indPred_mode",
              V6 = "V6"
            )
            x$finegrid$subfolder <- "Monolix"
            x$finegrid$file <- "finegrid.txt"
            x$finegrid$id <- "ID"
            x$finegrid$pattern <- "_obsVsPred"
            r <- read_mlx18_res(ipath, x$finegrid)
            expect_identical(r, NULL)
          })

  test_that("read_mlx18_res: 11params: path is wrong, subfolder is not empty,
          file exists; result: identical structure", {
            skip_on_os("windows")
            ipath <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline", "finegrid.txt")

            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$finegrid$names <- list(
              ID = "ID", time = "time", popPred = "popPred",
              "indPred_mean*" = "indPred_mean*", indPred_mode = "indPred_mode",
              V6 = "V6"
            )
            x$finegrid$subfolder <- "Monolix"
            x$finegrid$file <- "finegrid.txt"
            x$finegrid$id <- "ID"
            expect_s3_class(read_mlx18_res(ipath, x$finegrid), c("data.table", "data.frame"))
          })

  test_that("read_mlx18_res: params: path, occ = OCC and its in file; result: error", {
    skip_on_os("windows")
    my.file <- tempfile()
    fwrite(
      data.frame(
        "IDS" = c(1, 1, 1), "time" = c(0, 0.5, 1),
        "y1" = c(2000, 0, 0), "OCC" = c(0, 130, 228),
        "meanPred" = c(1, 0, 0), "indPred_mean*" = c(87, 87, 87),
        "indPred_mode" = c(125.3, 412.5, 658.63), "popWRes" = c(0.00, 0.52, 0.69),
        "meanWRes" = c(0.15, 0.21, 0.42), "indWRes_mean*" = c(0.15, 0.21, 0.42),
        "indWRes_mode" = c(121, 132, 145), "NPDE" = c(121, 132, 145), "OCC" = c(1, 0, 1)
      ),
      my.file
    )
    names. <- names(reader_help$conf$data)
    x <- reader_help$conf$data[names.]
    x$predictions$file <- basename(my.file)
    x$predictions$id <- "ID"
    expect_error(read_mlx18_res(dirname(my.file), x$predictions, occ = "OCC"))
  })

  test_that("read_mlx18_res: params: path is NULL, x;
          result: error", {
            skip_on_os("windows")
            ipath <- NULL
            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            expect_error(read_mlx18_res(ipath, x$predictions))
          })

  test_that("read_mlx18_res: params: path, x is NULL;
          result: error", {
            skip_on_os("windows")
            ipath <- file.path(reader_help$wd, "predictions.txt")
            x <- NULL
            expect_error(read_mlx18_res(ipath, x))
          })
  ##------------------- read_mlx18_res end-----------------------------------------

  #------------------- read_mlx_pred start ---------------------------------------
  test_that("read_mlx_pred: params: path, x; result: identical class", {
    ipath <- file.path(reader_help$wd, "predictions.txt")
    names. <- names(reader_help$conf$data)
    x <- reader_help$conf$data[names.]
    r <- read_mlx_pred(ipath, x$predictions)

    expect_true(inherits(r, "data.frame"))
  })

  test_that("read_mlx_pred: params: path, x$id is ID; result: identical class", {
    ipath <- file.path(reader_help$wd, "predictions.txt")
    names. <- names(reader_help$conf$data)
    x <- reader_help$conf$data[names.]
    x$id <- "ID"
    r <- read_mlx_pred(ipath, x$predictions)

    expect_true(inherits(r, "data.frame"))
  })

  test_that("read_mlx_pred: params: path, occ = OCC and its in file; result: identical class", {
    my.file <- tempfile()
    write.csv(
      data.frame(
        "IDS" = c(1, 1, 1), "time" = c(0, 0.5, 1),
        "y1" = c(2000, 0, 0), "popPred" = c(0, 130, 228),
        "meanPred" = c(1, 0, 0), "indPred_mean*" = c(87, 87, 87),
        "indPred_mode" = c(125.3, 412.5, 658.63), "popWRes" = c(0.00, 0.52, 0.69),
        "meanWRes" = c(0.15, 0.21, 0.42), "indWRes_mean*" = c(0.15, 0.21, 0.42),
        "indWRes_mode" = c(121, 132, 145), "NPDE" = c(121, 132, 145), "OCC" = c(1, 0, 1)
      ),
      my.file
    )
    names. <- names(reader_help$conf$data)
    x <- reader_help$conf$data[names.]
    x$id <- "IDS"
    r <- read_mlx_pred(my.file, x$predictions, occ = "OCC")
    expect_true(inherits(r, "data.frame"))
  })

  test_that("read_mlx_pred: params: path, occ = OCC but not such column in file;
          result: error", {
            my.file <- tempfile()
            write.csv(
              data.frame(
                "IDS" = c(1, 1, 1), "time" = c(0, 0.5, 1),
                "y1" = c(2000, 0, 0), "popPred" = c(0, 130, 228),
                "meanPred" = c(1, 0, 0), "indPred_mean*" = c(87, 87, 87),
                "indPred_mode" = c(125.3, 412.5, 658.63), "popWRes" = c(0.00, 0.52, 0.69),
                "meanWRes" = c(0.15, 0.21, 0.42), "indWRes_mean*" = c(0.15, 0.21, 0.42),
                "indWRes_mode" = c(121, 132, 145), "NPDE" = c(121, 132, 145)
              ),
              my.file
            )
            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$id <- "IDS"
            expect_error(read_mlx_pred(my.file, x$predictions, occ = "OCC"))
          })

  test_that("read_mlx_pred: params: path, x is NULL; result: error", {
    ipath <- file.path(reader_help$wd, "predictions.txt")
    x <- NULL

    expect_error(read_mlx_pred(ipath, x$predictions))
  })

  test_that("read_mlx_pred: params: path is NULL, x; result: error", {
    ipath <- NULL
    names. <- names(reader_help$conf$data)
    x <- reader_help$conf$data[names.]

    expect_error(read_mlx_pred(ipath, x$predictions))
  })
  #------------------- read_mlx_pred end------------------------------------------

  #------------------- read_mlx18_pred start -------------------------------------
  test_that("read_mlx18_pred: params: path, x;
          result: identical structure", {
            ipath <- file.path(reader_help$wd, "predictions.txt")
            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            r <- read_mlx18_pred(ipath, x$predictions)

            expect_identical(
              names(r),
              c("ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES")
            )
          })

  test_that("read_mlx18_pred: params: path is wrong, subfolder is not empty,
          file exists; result: identical structure", {
            ipath <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline", "predictions.txt")

            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$predictions$subfolder <- "Monolix"
            x$predictions$file <- "predictions.txt"
            r <- read_mlx18_pred(ipath, x$predictions)

            expect_identical(
              names(r),
              c("ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES")
            )
          })

  test_that("read_mlx18_pred: params:path is wrong, subfolder is not empty,
          file doesn't exist; result: NULL", {
            ipath <- file.path(system.file(package = "ggPMX"), "testdata", "predictions.txt")

            names. <- names(reader_help$conf$data)
            x <- reader_help$conf$data[names.]
            x$predictions$subfolder <- "theophylline"
            x$predictions$file <- "predictions.txt"
            r <- read_mlx18_pred(ipath, x$predictions)

            expect_identical(
              r,
              NULL
            )
          })

  test_that("read_mlx18_pred: params: residuals; result: error", {
    ipath <- file.path(reader_help$wd, "predictions.txt")
    names. <- names(reader_help$conf$data)
    x <- reader_help$conf$data[names.]
    x$predictions$residuals <- x$finegrid
    x$predictions$endpoint <- pmx_endpoint(
      code = "236",
      file.code = "2"
    )
    x$predictions$id <- "ID"
    expect_error(read_mlx18_pred(ipath, x$predictions))
  })
  #------------------- read_mlx18_pred end----------------------------------------

  #------------------- read_mlx_par_est start ------------------------------------
  test_that("read_mlx_par_est: params: path, x; result: identical class and structure", {
    ipath <- file.path(reader_help$wd, "estimates.txt")
    x <- NULL
    x$sep <- ";"
    r <- read_mlx_par_est(ipath, x)
    expect_identical(c("PARAM", "VALUE", "SE", "RSE", "PVALUE"), names(r))
    expect_true(inherits(r, "data.frame"))
  })

  test_that("read_mlx_par_est: params: path is NULL, x;
          result: error", {
            ipath <- NULL
            x <- NULL
            x$sep <- ";"

            expect_error(
              read_mlx_par_est(ipath, x)
            )
          })

  test_that("read_mlx_par_est: params: path, x is '' ; result: error", {
    ipath <- file.path(reader_help$wd, "estimates.txt")
    x <- NULL
    x$sep <- ""

    expect_error(read_mlx_par_est(ipath, x))
  })
  #------------------- read_mlx_par_est end---------------------------------------
}

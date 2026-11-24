test_that("Monolix record joins preserve duplicates and observation pairing", {
  predictions <- data.table::data.table(
    ID = c(1L, 1L, 2L),
    TIME = c(0, 0, 1),
    PRED = c(4, 4, 8),
    IPRED = c(5, 5, 9),
    IWRES = c(-1, 1, 0)
  )
  input <- data.table::data.table(
    ID = c(1L, 1L, 2L),
    TIME = c(0, 0, 1),
    DV = c(3, 7, 9)
  )
  residuals <- input[, c("ID", "TIME"), with = FALSE]
  residuals$npde_simBlq <- c(-2, 2, 0)
  simulated <- input[, c("ID", "TIME"), with = FALSE]
  simulated$y_simBlq_mode <- c(2, 6, 9)
  before <- data.table::copy(input)
  result <- post_load(
    list(
      predictions = predictions,
      sim_blq_npde_iwres = residuals,
      sim_blq_y = simulated
    ),
    input,
    "mlx18",
    NULL,
    ""
  )$data
  expect_equal(nrow(result$predictions), 3L)
  expect_equal(result$predictions$DV, c(3, 7, 9))
  expect_equal(result$predictions$IWRES, c(-1, 1, 0))
  expect_equal(nrow(result$sim_blq), 3L)
  expect_equal(result$sim_blq$DV, c(3, 7, 9))
  expect_equal(result$sim_blq$npde_simBlq, c(-2, 2, 0))
  expect_equal(result$sim_blq$y_simBlq_mode, c(2, 6, 9))
  expect_equal(input, before)
  expect_equal(any(grepl("pmx_record", names(result$sim_blq))), FALSE)
})

test_that("Monolix record joins warn and continue with incompatible repeated-key counts", {
  local_edition(3)
  x <- data.table::data.table(ID = c(1, 1), TIME = c(0, 0), PRED = c(2, 3))
  y <- data.table::data.table(ID = 1, TIME = 0, DV = 2)
  expect_snapshot(result <- merge_monolix_records(x, y, c("ID", "TIME")))
  expect_equal(nrow(result), 1L)
  expect_equal(result$PRED, 2)
  expect_equal(result$DV, 2)
  expect_snapshot(
    loaded <- post_load(list(predictions = x), y, "mlx18", NULL, "")
  )
  expect_equal(loaded$data$predictions, result)
})

test_that("Monolix readers retain residual pairing, time precision and occasions", {
  root <- tempfile()
  dir.create(
    file.path(root, "ChartsData", "ScatterPlotOfTheResiduals"),
    recursive = TRUE
  )
  on.exit(unlink(root, recursive = TRUE))
  path <- file.path(root, "predictions.txt")
  data.table::fwrite(
    data.table::data.table(
      id = c(1, 1, 1),
      time = c(0.123456, 0.123456, 0.123456),
      occasion = c(1, 1, 2),
      poppred = c(4, 4, 8),
      indivpred_mode = c(5, 5, 9),
      indwres_mode = c(-1, 1, 0)
    ),
    path
  )
  data.table::fwrite(
    data.table::data.table(
      ID = c(1, 1, 1),
      time = c(0.123456, 0.123456, 0.123456),
      occasion = c(1, 1, 2),
      npde = c(-2, 2, 0)
    ),
    file.path(
      root,
      "ChartsData",
      "ScatterPlotOfTheResiduals",
      "y_residuals.txt"
    )
  )
  config <- load_config("standing", "mlx18")$data$predictions
  result <- read_mlx18_pred(path, config, occ = "occasion")
  expect_equal(nrow(result), 3L)
  expect_equal(result$TIME, rep(0.1235, 3))
  expect_equal(result$OCC, c(1, 1, 2))
  expect_equal(result$IWRES, c(-1, 1, 0))
  expect_equal(result$NPDE, c(-2, 2, 0))
})

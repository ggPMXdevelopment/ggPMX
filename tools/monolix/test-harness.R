library(testthat)
source("helpers.R")

test_that("controller and plot checks accept the packaged example", {
  ctr <- ggPMX::theophylline()
  estimates <- ggPMX::get_data(ctr, "estimates")
  reference <- data.frame(parameter = trimws(estimates$PARAM), value = estimates$VALUE)
  checked <- check_demo_controller(ctr, reference)
  output <- Sys.getenv("GGPMX_HARNESS_TEST_OUTPUT")
  if (!nzchar(output)) stop("Set GGPMX_HARNESS_TEST_OUTPUT to a scratch directory")
  check_demo_plot(
    ggPMX::pmx_plot_dv_pred(ctr, is.smooth = FALSE), checked$predictions,
    "PRED", "DV", file.path(output, "harness-dv-pred.png")
  )
})

test_that("copy_demo does not accept an incomplete project", {
  error <- tryCatch(
    copy_demo("missing/project.mlxtran", Sys.getenv("GGPMX_HARNESS_TEST_OUTPUT")),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Demo requires its project, data, and fitted results", fixed = TRUE)
})

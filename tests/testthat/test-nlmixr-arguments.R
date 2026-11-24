test_that("nlmixr controller handles a missing fit without loading nlmixr", {
  expect_null(pmx_nlmixr())
  expect_null(pmx_nlmixr(conts = c("cl", "v")))
  error <- tryCatch(pmx_plot_individual(pmx_nlmixr()), error = identity)
  expect_s3_class(error, "error")
})

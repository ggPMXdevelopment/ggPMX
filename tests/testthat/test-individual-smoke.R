test_that("individual plots retain basic pagination and customization coverage", {
  ctr <- test_pmxClass_helpers()$ctr
  expect_s3_class(pmx_plot_individual(ctr, which_pages = 1), "ggplot")
  customized <- pmx_plot_individual(
    ctr,
    which_pages = 1,
    point = list(shape = 24),
    is.legend = FALSE
  )
  expect_s3_class(customized, "ggplot")
  expect_s3_class(get_plot(ctr, "individual", 1), "ggplot")
})

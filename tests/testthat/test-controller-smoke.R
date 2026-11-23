test_that("controller import, getters, and representative plots work", {
  ctr <- test_pmxClass_helpers()$ctr
  expect_s3_class(ctr, "pmxClass")
  expect_identical(get_cats(ctr), "SEX")
  expect_identical(get_conts(ctr), c("WT0", "AGE0"))
  expect_identical(get_strats(ctr), "STUD")
  expect_s3_class(get_data(ctr, "eta"), "data.frame")
  expect_contains(plot_names(ctr), c("dv_pred", "npde_time", "individual"))
  expect_named(plots(ctr), c("plot_name", "plot_type", "plot_function"))

  for (name in c("dv_pred", "npde_time")) {
    wrapped <- pmx_plot_generic(ctr, pname = name)
    direct <- get_plot(ctr, name)
    expect_s3_class(wrapped, "ggplot")
    expect_s3_class(direct, "ggplot")
    expect_identical(wrapped$plot_env$aess, direct$plot_env$aess)
  }
  expect_s3_class(
    pmx_plot_dv_pred(ctr, point = list(shape = 24), is.title = FALSE),
    "ggplot"
  )
})

test_that("custom configuration loads without importing model data", {
  config <- pmx_config(
    plots = system.file("examples", "plots.yaml", package = "ggPMX"),
    inputs = system.file("examples", "custom_inputs.yaml", package = "ggPMX")
  )
  expect_s3_class(config, "pmxConfig")
  expect_named(config, c("data", "plots", "sys", "hasNpd"))
})

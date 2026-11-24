test_that("focused controller fixtures preserve imported data and plot output", {
  full <- theophylline()
  focused <- test_controller("DV_PRED")

  expect_null(focused$sim)
  expect_setequal(plot_names(focused), "dv_pred")
  expect_setequal(names(focused$data), setdiff(names(full$data), "sim"))
  for (name in names(focused$data)) {
    expect_equal(focused$data[[name]], full$data[[name]])
  }
  expect_equal(focused$input, full$input)
  expect_equal(focused$settings, full$settings)
  expect_equal(
    ggplot2::ggplot_build(pmx_plot_dv_pred(focused))$data,
    ggplot2::ggplot_build(pmx_plot_dv_pred(full))$data
  )
})

test_that("focused controller fixtures are independently imported", {
  first <- test_controller()
  second <- test_controller()
  expect_length(plot_names(first), 0)
  data.table::set(first$input, j = "fixture_only", value = TRUE)
  data.table::set(first$data$eta, j = "fixture_only", value = TRUE)
  expect_equal("fixture_only" %in% names(second$input), FALSE)
  expect_equal("fixture_only" %in% names(second$data$eta), FALSE)
})

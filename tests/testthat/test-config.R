context("Test config")

test_that("can source configs", {
  conf <- configs()
  expect_identical(conf$name, c("standing"))
})



test_that("can load configs", {
  conf <- configs()
  cfig <- load_config(conf$name[1], "mlx")
  expect_s3_class(cfig, "pmxConfig")
})

test_that("can print loaded config", {
  conf <- configs()
  cfig <- load_config(conf$name[1], "mlx")
  expect_output(print(cfig), "data_name")
  expect_output(print(cfig), "plot_name")
})


test_that("raise error if bad config name is provided", {
  expect_error(load_config("BAD_CONFIG_NAME"))
})

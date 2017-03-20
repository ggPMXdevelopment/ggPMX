context("Test config")

test_that("can source configs", {
  conf <- configs()
  expect_identical(conf$name, c("config1", "standing"))
})

test_that("can print configs", {
  conf <- configs()
  expect_output(print(conf), "There are 2 configs for mlx system ")
})

test_that("can load configs", {
  conf <- configs()
  cfig <- load_config(conf$name[1], "mlx")
  expect_s3_class(cfig, "pmxConfig")
})

test_that("can print loaded config", {
  conf <- configs()
  cfig <- load_config(conf$name[1], "mlx")
  expect_output(print(cfig), "configuration Object")
})

context("Test reader parameters")
reader_help <- reader_helpers()

test_that("load standing config", {
  expect_silent(load_config("standing","mlx"))
  expect_indentical(reader_help$conf$sys, "mlx")
})

test_that("can read mlx ind", {
  
})


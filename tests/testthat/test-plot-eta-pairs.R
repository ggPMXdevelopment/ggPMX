library(ggPMX)
library(purrr)
ctr <- theophylline()

context("Test plot-eta-pairs with reference lines at 0, -1.96 and 1.96")

#------------------- pmx_plot_eta_matrix start ------------------------------------------
test_that("pmx_plot_eta_matrix: params: is.vreference_line, vreference_line (value by
   default) result: identical value of params", {
  p <- ctr %>% pmx_plot_eta_matrix(is.vreference_line = TRUE)
  expect_identical(p$plots$`Cl;Cl`$layers[[2]]$aes_params$colour, "orange")
  expect_identical(p$plots$`Cl;Cl`$layers[[2]]$aes_params$linetype, "longdash")
  expect_identical(p$plots$`V;V`$layers[[2]]$aes_params$colour, "orange")
  expect_identical(p$plots$`V;V`$layers[[2]]$aes_params$linetype, "longdash")
  expect_identical(p$plots$`ka;ka`$layers[[2]]$aes_params$colour, "orange")
  expect_identical(p$plots$`ka;ka`$layers[[2]]$aes_params$linetype, "longdash")
})

test_that("pmx_plot_eta_matrix: params: is.vreference_line, vreference_line result:
    identical value of params", {
  p <- pmx_plot_eta_matrix(
    ctr,
    is.vreference_line = TRUE,
    vreference_line = list(colour = "blue", linetype = "longdash")
  )

  expect_identical(p$plots$`Cl;Cl`$layers[[2]]$aes_params$colour, "blue")
  expect_identical(p$plots$`Cl;Cl`$layers[[2]]$aes_params$linetype, "longdash")
  expect_identical(p$plots$`V;V`$layers[[2]]$aes_params$colour, "blue")
  expect_identical(p$plots$`V;V`$layers[[2]]$aes_params$linetype, "longdash")
  expect_identical(p$plots$`ka;ka`$layers[[2]]$aes_params$colour, "blue")
  expect_identical(p$plots$`ka;ka`$layers[[2]]$aes_params$linetype, "longdash")
})

test_that("pmx_plot_eta_matrix: params: is.vreference_line, vreference_line etc. result:
    lack of parameters of vreference_line", {
  p <- ctr %>% pmx_plot_eta_matrix(is.vreference_line = FALSE)
  expect_true(is_empty(p$plots$`Cl;Cl`$layers[[2]]$aes_params$colour))
  expect_true(is_empty(p$plots$`Cl;Cl`$layers[[2]]$aes_params$linetype))
  expect_true(is_empty(p$plots$`V;V`$layers[[2]]$aes_params$colour))
  expect_true(is_empty(p$plots$`V;V`$layers[[2]]$aes_params$linetype))
  expect_true(is_empty(p$plots$`ka;ka`$layers[[2]]$aes_params$colour))
  expect_true(is_empty(p$plots$`ka;ka`$layers[[2]]$aes_params$linetype))
})
#------------------- pmx_plot_eta_matrix end ------------------------------------------

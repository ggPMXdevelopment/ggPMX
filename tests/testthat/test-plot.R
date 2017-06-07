

test_that("can create a plot using setting dname", {
  
  ctr <- pmx_mlx("standing")
  ctr %>% set_plot("DIS", pname = "distr1", type = "box",dname="eta")
  p <- ctr %>% get_plot("distr1")
  pconf <- ggplot2::ggplot_build(p)
  expect_identical(dim(pconf$data[[2]]), c(150L, 10L))
})


test_that("Create a plot with not valid dname throw error", {
  
  ctr <- pmx_mlx("standing")
  expect_error(
    ctr %>% set_plot("DIS", pname = "distr1", type = "box",dname="xxx")
  )

})
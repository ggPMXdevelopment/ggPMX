context("Test VPC plot")

test_that("pmx_plot_vpc: params: ctr, is.footnote; result: ggplot", {
  ctr <- theophylline()
  p <- pmx_plot_vpc(ctr, is.footnote=FALSE)
  expect_s3_class(p, 'ggplot')
})

test_that("pmx_plot_vpc: params: ctr, strat.facet; result: ggplot", {
  ctr <- theophylline()
  p <- pmx_plot_vpc(ctr, strat.facet = ~STUD)
  expect_s3_class(p, 'ggplot')
})

test_that("pmx_plot_vpc: params: ctr; result: ggplot", {
  ctr <- theophylline()
  p <- pmx_plot_vpc(ctr)
  expect_s3_class(p, 'ggplot')
})

test_that("pmx_plot_vpc: params: ctr, bin; result: ggplot", {
  ctr <- theophylline()
  p <- pmx_plot_vpc(ctr, bin=pmx_vpc_bin(style="equal"))
  expect_s3_class(p, 'ggplot')
})

test_that("custom labels are applied to pmx_plot_vpc", {
  ctr <- theophylline()
  p <- pmx_plot_vpc(ctr, labels = c(x = "custom axis x", y = "custom axis y"))
  expect_identical(p[["labels"]][["x"]], "custom axis x")
  expect_identical(p[["labels"]][["y"]], "custom axis y")
})

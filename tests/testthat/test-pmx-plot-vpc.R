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

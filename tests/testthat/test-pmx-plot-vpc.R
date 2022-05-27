context("Test VPC plot")

test_that("pmx_plot_vpc: params: ctr, is.footnote; result: ggplot", {
  ctr <- theophylline()
  p <- pmx_plot_vpc(ctr, is.footnote=FALSE)
  expect_s3_class(p, 'ggplot')
})

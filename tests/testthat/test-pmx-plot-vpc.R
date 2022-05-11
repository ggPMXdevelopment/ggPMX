context("Test VPC plot")

test_that("pmx_plot_vpc: params: ctr, strat.facet; result: ggplot", {
  ctr <- theophylline()
  p <- pmx_plot_vpc(ctr, strat.facet = ~STUD)
  expect_s3_class(p, 'ggplot')
})

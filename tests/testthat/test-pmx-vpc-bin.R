if (helper_skip()) {

  library(ggPMX)
  ctr <- theophylline()

  context("Test pmx_vpc_bin function")

  test_that("pmx_vpc_bin: params result: identical type", {
    p <- ctr %>% pmx_vpc_bin()
    expect_true(inherits(p, "list"))
    expect_identical(pmx_vpc_bin(), NULL)
  })


  test_that("pmx_vpc_bin: params result:  ", {
    p <- ctr %>% pmx_vpc_bin(style = "equal")
    styles <- c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "jenks")
    expect_true(is.element(p$style, styles))
  })


  test_that("pmx_vpc_bin: params result: error", {
    expect_error(ctr %>% pmx_vpc_bin(style = c("sd", "jenks"), within_strat = FALSE))
  })


  test_that("pmx_vpc_bin: params result: identical type", {
    p <- pmx_vpc_bin()
    expect_identical(p, NULL)
  })
}

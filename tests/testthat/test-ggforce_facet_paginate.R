context("Test ggforce facet pagination")

test_that("can call facet_wrap_paginate", {
  n_pages <- ceiling(
    length(levels(ggplot2::diamonds$cut)) * 
      length(levels(ggplot2::diamonds$clarity)) / 9
  )
  
  g <- ggplot2::ggplot(ggplot2::diamonds) +
    ggplot2::geom_point(ggplot2::aes(carat, price), alpha = 0.1) +
    facet_wrap_paginate(~cut:clarity, ncol = 3, nrow = 3, page = 1)
  expect_silent(g)
})


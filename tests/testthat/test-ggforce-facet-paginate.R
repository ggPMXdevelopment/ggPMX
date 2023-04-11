context("Test spliting facet_wrap over multiple plots")

#------------------- facet_wrap_paginate start --------------------------------

test_that("facet_wrap_paginate: params: nrow and ncol is not NULL
          result: FacetWrapPaginate",
          {
            dx <- pmx_qq("AGE0")
            strat.facet <- dx[["strat.facet"]]
            facets <- wrap_formula(strat.facet, "EFFECT")
            expect_true(inherits(
              ggforce::facet_wrap_paginate(facets, nrow = 2, ncol = 3),
              "FacetWrapPaginate"
            ))
          })

test_that("facet_wrap_paginate: params: ncol is not NULL and nrow is NULL
          result: FacetWrap", {
  dx <- pmx_qq("AGE0")
  strat.facet <- dx[["strat.facet"]]
  facets <- wrap_formula(strat.facet, "EFFECT")
  expect_true(inherits(ggforce::facet_wrap_paginate(facets, ncol = 3), "FacetWrap"))
})

test_that("facet_wrap_paginate: params: nrow and ncol is NULL
          result: FacetWrap", {
  dx <- pmx_qq("AGE0")
  strat.facet <- dx[["strat.facet"]]
  facets <- wrap_formula(strat.facet, "EFFECT")
  expect_true(inherits(ggforce::facet_wrap_paginate(facets), "FacetWrap"))
})

test_that("facet_wrap_paginate: params: ncol is NULL and nrow is not NULL
          result: FacetWrap", {
  dx <- pmx_qq("AGE0")
  strat.facet <- dx[["strat.facet"]]
  facets <- wrap_formula(strat.facet, "EFFECT")
  expect_true(inherits(ggforce::facet_wrap_paginate(facets, nrow = 2), "FacetWrap"))
})

test_that("facet_wrap_paginate: params: all params are NULL result: error", {
  expect_error(ggforce::facet_wrap_paginate())
})
#------------------- facet_wrap_paginate end ----------------------------------

#------------------- n_pages start --------------------------------------------

test_that("n_pages: params: plot without pages result: NULL", {
  ctr <- theophylline()
  plot <- pmx_plot_individual(ctr, which_pages = 1)
  expect_true(is.null(n_pages(plot)))
})

test_that("n_pages: params: no result: NULL", {
  expect_error(n_pages())
})
#------------------- n_pages end ----------------------------------------------

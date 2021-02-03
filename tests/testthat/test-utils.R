context("Test utility functions")

test_that("merge vectors error works", {
  expect_error(
    mergeVectors.(1:4, 5:8),
    "Vectors must be either NULL or have names for all elements"
  )
})

test_that("l_left_join merge compound lists", {
  res <-
    l_left_join(
      list(
        x = 1,
        y = 1,
        h = list(z = 1)
      ),
      list(
        y = 2,
        h = list(h = 4)
      )
    )
  expected <- list(x = 1, h = list(z = 1, h = 4), y = 2)
  expect_identical(res, expected)
})

test_that("pk_pd is worrking", {
  ctr <- pk_pd()
  expect_s3_class(ctr, "pmxClass")
})

#------------------- facet_grid_scale start --------------------------------------
test_that(
  "facet_grid_scale: params: scales = 'sym', scale_y = NULL result:
          FacetGridScales object",
  {
    expect_true(inherits(
      facet_grid_scale(
        stats::as.formula("EFFECT~variable"),
        scales = "sym",
        scale_y = NULL
      ),
      "FacetGridScales"
    ))
  }
)

test_that(
  "facet_grid_scale: params: scales = 'free', scale_y = NULL result:
          FacetGridScales object",
  {
    expect_true(inherits(
      facet_grid_scale(
        stats::as.formula("EFFECT~variable"),
        scales = "free",
        scale_y = NULL
      ),
      "FacetGridScales"
    ))
  }
)

test_that(
  "facet_grid_scale: params: scales = 'free_y', scale_y = NULL result:
          FacetGridScales object",
  {
    expect_true(inherits(
      facet_grid_scale(
        stats::as.formula("EFFECT~variable"),
        scales = "free_y",
        scale_y = NULL
      ),
      "FacetGridScales"
    ))
  }
)

test_that(
  "facet_grid_scale: params: scales = 'free_x', scale_y = NULL result:
          FacetGridScales object",
  {
    expect_true(inherits(
      facet_grid_scale(
        stats::as.formula("EFFECT~variable"),
        scales = "free_x",
        scale_y = NULL
      ),
      "FacetGridScales"
    ))
  }
)

test_that("facet_grid_scale: params: scales = 'free_z', scale_y = NULL result:
          error",
          {
            expect_error(facet_grid_scale(
              stats::as.formula("EFFECT~variable"),
              scales = "free_z",
              scale_y = NULL
            ))
          })

test_that("facet_grid_scale: params: scales = 'sym', scale_y = 0 result:
          error",
          {
            expect_error(facet_grid_scale(
              stats::as.formula("EFFECT~variable"),
              scales = "free_z",
              scale_y = 0
            ))
          })

test_that("facet_grid_scale: params: scales = 'sym', scale_y is not NULL result:
          ggproto object",
          {
            mlxpath <- file.path(system.file(package = "ggPMX"),
                                 "testdata",
                                 "1_popPK_model",
                                 "project.mlxtran")
            ctr <- pmx_mlxtran(mlxpath, config = "standing")
            x <- eta_cov(
              labels = list("SEX", "Disease"),
              type = "cats",
              covariates = pmx_cov(
                values = list("SEX", "DISE"),
                labels = list("SEX", "Disease")
              ),
              scale = "sym"
            )
            dx <- ctr %>% get_data("eta")
            df <-
              unique(dx[, c("SEX", "RACE", "DISE",  "VALUE", "EFFECT"), with = FALSE])
            df <-
              df %>% dplyr::select(unlist(x$covariates$values), "VALUE", "EFFECT")
            scale_y <- get_scale_y(df)
            expect_true(inherits(
              facet_grid_scale(
                stats::as.formula("EFFECT~variable"),
                scales = x$scale,
                scale_y = scale_y
              ),
              "ggproto"
            ))
          })

test_that("facet_grid_scale: params: scales = 'free', scale_y is not NULL result:
          ggproto object",
          {
            mlxpath <- file.path(system.file(package = "ggPMX"),
                                 "testdata",
                                 "1_popPK_model",
                                 "project.mlxtran")
            ctr <- pmx_mlxtran(mlxpath, config = "standing")
            x <- eta_cov(
              labels = list("SEX", "Disease"),
              type = "cats",
              covariates = pmx_cov(
                values = list("SEX", "DISE"),
                labels = list("SEX", "Disease")
              ),
              scale = "free"
            )
            dx <- ctr %>% get_data("eta")
            df <-
              unique(dx[, c("SEX", "RACE", "DISE",  "VALUE", "EFFECT"), with = FALSE])
            df <-
              df %>% dplyr::select(unlist(x$covariates$values), "VALUE", "EFFECT")
            scale_y <- get_scale_y(df)
            expect_true(inherits(
              facet_grid_scale(
                stats::as.formula("EFFECT~variable"),
                scales = x$scale,
                scale_y = scale_y
              ),
              "ggproto"
            ))
          })
#------------------- facet_grid_scale end ----------------------------------------

#------------------- get_facets_list start ---------------------------------------
test_that(
  "get_facets_list: params: row is formula result:
          list object",
  {
    expect_true(inherits(
      get_facets_list(stats::as.formula("EFFECT~variable")),
      "list"
    ))
  }
)

test_that(
  "get_facets_list: params: row is NULL result:
          list object",
  {
    expect_true(inherits(
      get_facets_list(NULL),
      "list"
    ))
  }
)

test_that(
  "get_facets_list: params: row is formula result:
          equal values",
  {
    expect_equal(length(
      get_facets_list(stats::as.formula("EFFECT~variable"))),
      2
    )
  }
)
#------------------- get_facets_list end -----------------------------------------

#------------------- get_facets_of_axis start ------------------------------------
test_that(
  "get_facets_of_axis: params: row is formula result:
          list object",
  {
    expect_true(inherits(
      get_facets_of_axis(stats::as.formula("EFFECT~variable")),
      "list"
    ))
  }
)

test_that(
  "get_facets_of_axis: params: row is NULL result:
          list object",
  {
    expect_true(inherits(
      get_facets_of_axis(NULL),
      "list"
    ))
  }
)

test_that(
  "get_facets_of_axis: params: row is formula result:
          equal values",
  {
    expect_equal(length(
      get_facets_of_axis(stats::as.formula("EFFECT~variable"))),
      2
    )
  }
)
#------------------- get_facets_of_axis end --------------------------------------

#------------------- as_facets start ---------------------------------------------
test_that(
  "as_facets: params: row is formula result:
          quosures object",
  {
    expect_true(inherits(
      as_facets("EFFECT"),
      "quosures"
    ))
  }
)

test_that(
  "as_facets: params: row is NULL result:
          list object",
  {
    expect_true(inherits(
      as_facets(NULL),
      "quosures"
    ))
  }
)
#------------------- as_facets end -----------------------------------------------

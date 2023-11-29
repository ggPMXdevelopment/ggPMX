if (helper_skip()) {

  context("Test structure of a graphic distribution object")

  #------------------- distrib start ------------------------------------------
  test_that("distrib: params: labels and facets etc. result:
          identical structure", {
            labels <- list("EVID", "SEX")
            facets <- list(nrow = 5, ncol = 5)
            expect_identical(
              distrib(
                labels,
                is.shrink = FALSE,
                type = "hist",
                facets = facets,
                dname = "predictions"
              ),
              structure(
                list(
                  ptype = "DIS",
                  strat = TRUE,
                  dname = "predictions",
                  aess = list(x = "EFFECT", y = "VAR", z = "FUN"),
                  type = "hist",
                  is.jitter = FALSE,
                  jitter = NULL,
                  facets = facets,
                  histogram = NULL,
                  is.shrink = FALSE,
                  shrink = NULL,
                  gp = pmx_gpar(
                    labels = labels,
                    discrete = TRUE,
                    is.smooth = FALSE
                  )
                ),
                class = c("distrib", "pmx_gpar")
              )
            )
          })


  test_that("distrib: params: labels and facets etc. result: distrib object", {
    labels <- list("EVID", "SEX")
    facets <- list(nrow = 5, ncol = 5)
    expect_true(inherits(
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = facets,
        dname = "predictions"
      ),
      "distrib"
    ))
  })

  test_that("distrib: params: labels is Null; result: error", {
    labels <- NULL
    facets <- list(nrow = 5, ncol = 5)
    expect_error(
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = facets,
        dname = "predictions"
      )
    )
  })

  test_that("distrib: params: integer facets; result: error", {
    labels <- list("EVID", "SEX")
    facets <- 2
    expect_error(
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = facets,
        dname = "predictions"
      )
    )
  })
  #------------------- distrib end ---------------------------------------------

  #------------------- wrap_formula start --------------------------------------

  test_that("wrap_formula: params: x is a formula result: formula", {
    x <- ~ a + y + z
    expect_true(inherits(wrap_formula(x), "formula"))
  })

  test_that("wrap_formula: params: x is integer result: formula", {
    x <- 10L
    expect_true(inherits(wrap_formula(x), "formula"))
  })

  test_that("wrap_formula: params: x is NULL result: not formula", {
    x <- NULL
    expect_true(inherits(wrap_formula(x), "formula"))
  })

  test_that("wrap_formula: params: x is NA result: not formula", {
    x <- NA
    expect_true(inherits(wrap_formula(x), "formula"))
  })

  test_that("wrap_formula: params: x is string result: not formula", {
    x <- "hello you"
    expect_error(wrap_formula(x))
  })
  #------------------- wrap_formula end ----------------------------------------

  #------------------- jitter_layer start --------------------------------------

  test_that("jitter_layer: params: jitter and strat.color result:
          LayerInstance", {
            expect_true(inherits(
              jitter_layer(
                jitter = list(alpha = 0.4, color = "red"),
                strat.color = "SEX"
              ),
              "LayerInstance"
            ))
          })

  test_that("jitter_layer: params: strat.color equals NULL result:
          LayerInstance", {
            expect_true(inherits(
              jitter_layer(
                jitter = list(alpha = 0.4, color = "red"),
                strat.color = NULL
              ),
              "LayerInstance"
            ))
          })

  test_that("jitter_layer: params: jitter and strat.color equal NULL result:
          error", {
            expect_error(jitter_layer(jitter = NULL, strat.color = NULL))
          })

  test_that("jitter_layer: params: no result: error", {
    expect_error(jitter_layer())
  })
  #------------------- jitter_layer end ----------------------------------------

  #------------------- distrib.hist start --------------------------------------
  test_that("distrib.hist: params: labels, type and etc. result: ggplot", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("eta")
    labels <- list("EVID", "SEX")
    x <-
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = NULL,
        dname = "predictions"
      )
    expect_true(inherits(
      distrib.hist(
        dx,
        strat.facet = dx$ID,
        strat.color = dx$SEX,
        x
      ),
      "gg"
    ))
  })

  test_that("distrib.hist: params: x equals NULL result: error", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("eta")
    labels <- list("EVID", "SEX")
    x <- NULL
    expect_error(distrib.hist(
      dx,
      strat.facet = dx$ID,
      strat.color = dx$SEX,
      x
    ))
  })

  test_that("distrib.hist: params: dx equals NULL result: error", {
    dx <- NULL
    labels <- list("EVID", "SEX")
    x <-
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = NULL,
        dname = "predictions"
      )
    expect_error(distrib.hist(
      dx,
      strat.facet = dx$ID,
      strat.color = dx$SEX,
      x
    ))
  })

  #------------------- distrib.hist end ---------------------------------------

  #------------------- distrib.box start --------------------------------------

  test_that("distrib.box: params: labels, type, etc. result: ggplot", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("eta")
    labels <- list("EVID", "SEX")
    x <-
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = NULL,
        dname = "predictions"
      )
    expect_true(inherits(
      distrib.box(dx, strat.color = dx$SEX, strat.facet = NULL, x),
      "gg"
    ))
  })

  test_that("distrib.box: params: x equals NULL result: error", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("eta")
    labels <- list("EVID", "SEX")
    x <- NULL
    expect_error(distrib.box(
      dx,
      strat.color = dx$SEX,
      strat.facet = dx$ID,
      x
    ))
  })

  test_that("distrib.box: params: dx equals NULL result: gg", {
    dx <- NULL
    labels <- list("EVID", "SEX")
    x <-
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = NULL,
        dname = "predictions"
      )
    expect_true(inherits(
      distrib.box(dx, strat.color = dx$SEX, strat.facet = NULL, x),
      "gg"
    ))
  })

  test_that("distrib.box: params: strat.facet is not NULL result: gg", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("eta")
    labels <- list("EVID", "SEX")
    x <-
      distrib(
        labels,
        is.shrink = FALSE,
        type = "hist",
        facets = NULL,
        dname = "predictions"
      )
    expect_true(inherits(
      distrib.box(
        dx,
        strat.color = dx$SEX,
        strat.facet = ~SEX,
        x
      ),
      "gg"
    ))
  })

  #------------------- distrib.box end ----------------------------------------

  #------------------- shrinkage_layer start ----------------------------------

  test_that("shrinkage_layer: params: hist type result: LayerInstance", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("estimate")
    expect_true(inherits(
      shrinkage_layer(
        dx,
        shrink=list(hjust=0.5, fun="var"),
        type = "hist",
        strat.color = dx$SEX
      ),
      "LayerInstance"
    ))
  })

  test_that("shrinkage_layer: params: dx contain eta data result: warning", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("eta")
    expect_warning(shrinkage_layer(
      dx,
      shrink = list(hjust = 0.5),
      type = "hist",
      strat.color = dx$SEX
    ))
  })

  test_that("shrinkage_layer: params: box type result: LayerInstance", {
    ctr <- theophylline()
    dx <- ctr %>% get_data("estimate")
    expect_true(inherits(
      shrinkage_layer(
        dx,
        shrink=list(hjust=0.5, fun="var"),
        type = "box",
        strat.color = dx$SEX
      ),
      "LayerInstance"
    ))
  })

  #------------------- shrinkage_layer end ------------------------------------

  #------------------- plot_distribution start --------------------------------
  test_that("plot_distribution: params: dx contain eta data,
          x is distrib object result: error", {
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            labels <- list("EVID", "SEX")
            x <-
              distrib(
                labels,
                is.shrink = FALSE,
                type = "hist",
                facets = list("SEX"),
                dname = "predictions"
              )
            expect_error(plot_distribution(x, dx))
          })
  #------------------- plot_distribution end ----------------------------------

  #------------------- plot_pmx.distrib start ---------------------------------
  test_that("plot_pmx.distrib: params: dx contain eta data,
          x is distrib object result: error", {
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            labels <- list("EVID", "SEX")
            x <-
              distrib(
                labels,
                is.shrink = FALSE,
                type = "hist",
                facets = list("SEX"),
                dname = "predictions"
              )
            expect_error(plot_pmx.distrib(x, dx))
          })
  #------------------- plot_pmx.distrib end ------------------------------------
}

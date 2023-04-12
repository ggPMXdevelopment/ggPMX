context("Test pmx_plot_eta_cats function")
ctr <- theophylline()
#------------------- pmx_plot_eta_cats start ------------------------------------------
test_that(
  "pmx_plot_eta_cats: params: strat.color; result: identical value",
  {
    p <- ctr %>% pmx_plot_eta_cats(is.strat.color = TRUE)
    expect_equal(p$plot_env$x$is.strat.color, TRUE)
  }
)

test_that("levels filtering is working", {
  p <- pmx_plot_eta_cats(
    ctr,
    effects=list(
      levels=c("ka", "V"),
      labels=c("Absorption_rate", "V")
    )
  )

  expect_identical(levels(p[["data"]][["EFFECT"]]), c("Absorption_rate", "V"))
})

#------------------- pmx_plot_eta_cats end --------------------------------------------

context("Test pmx Eta Covariates plots")

ctr <- theophylline(settings = pmx_settings(effects = list(
  levels = c("ka", "V", "Cl"),
  labels = c("Concentration", "Volume", "Clearance")
)))

#------------------- pmx_plot_eta_cats start -------------------------------
test_that(
  "pmx_plot_eta_cats: params: ctr is controller, covariates result: gg",
  {
    expect_true(inherits(
      pmx_plot_eta_cats(
        ctr = ctr,
        covariates = pmx_cov(
          values = list("WT0", "AGE0"),
          labels = list("Weight", "Age")
        )
      ),
      "gg"
    ))
  }
)

test_that(
  "pmx_plot_eta_cats: params: ctr is controller, covariates result: gg",
  {
    expect_error(ctr <- theophylline(settings = pmx_settings(effects = list(
      levels = c("ka", "V"),
      labels = c("Concentration", "Volume")
    ))), NA)
  }
)

test_that(
  "pmx_plot_eta_cats: params: ctr is controller, covariates result: list",
  {
    expect_true(all(ctr$settings$effects$levels == c("ka", "V", "Cl")))

    expect_true(all(
      ctr$settings$effects$labels == c("Concentration", "Volume", "Clearance")
    ))
    p <- pmx_plot_eta_cats(
      ctr = ctr,
      covariates = pmx_cov(
        values = list("WT0", "AGE0"),
        labels = list("Weight", "Age")
      )
    )
    expect_true(all(unique(p$data$variable) %in% c("Weight", "Age")))
  }
)
#------------------- pmx_plot_eta_cats end ---------------------------------

#------------------- pmx_plot_eta_conts start ------------------------------
test_that(
  "pmx_plot_eta_conts: params: ctr is controller, covariates result: gg",
  {
    expect_true(inherits(
      pmx_plot_eta_conts(
        ctr = ctr,
        covariates = pmx_cov(
          values = list("WT0", "AGE0"),
          labels =
            list("Weight", "Age")
        )
      ),
      "gg"
    ))
  }
)
#------------------- pmx_plot_eta_conts end --------------------------------

if (helper_skip()) {

  context("Test update plots")
  helpers <- helper_updateplots()

  test_that("can update DIS plot", {
    ctr <- helpers$ctr
    ctr %>% set_plot("DIS", pname = "distr1", type = "box", is.shrink = FALSE)
    expect_true("distr1" %in% ctr$plots())
    p <- ctr %>% get_plot("distr1")
    oldconf <- ctr$get_config("distr1")
    expect_false(oldconf$is.shrink)
  })

  test_that("can remove DIS plot", {
    ctr <- helpers$ctr
    ctr$remove_plot("distr1")
    expect_false("distr1" %in% ctr$plots())
  })


  test_that("can update IND plot", {
    ctr <- helpers$ctr
    ctr %>% set_plot("IND", pname = "indiv1")
    expect_is(ctr %>% get_plot("indiv1", c(1, 2)), "list")
    expect_true("indiv1" %in% ctr$plots())
    oldconf <- ctr$get_config("indiv1")
    expect_false(oldconf$gp$is.band)

    ctr %>% pmx_update("indiv1", is.band = TRUE)
    newconf <- ctr$get_config("indiv1")
    expect_true(newconf$gp$is.band)
  })


  test_that("can remove IND plot", {
    ctr <- helpers$ctr
    ctr$remove_plot("indiv1")
    expect_false("indiv1" %in% ctr$plots())
  })


  test_that("can update with filter", {
    # set new plot
    ctr <- helpers$ctr
    ctr %>% set_plot("DIS", pname = "distr1", type = "box")
    ctr %>% get_plot("distr1")
    p <- ctr %>% get_plot("distr1")
    pconf <- ggplot2::ggplot_build(p)
    expect_equal(length(pconf$data), 5)

    # Update plot with filter
    ctr %>% pmx_update("distr1", filter = ID < 10)
    p <- ctr %>% get_plot("distr1")
    pconf <- ggplot2::ggplot_build(p)
    expect_equal(length(pconf$data), 5)

    # test can remove filter
    ctr %>% pmx_update("distr1", filter = NULL)
    p <- ctr %>% get_plot("distr1")
    pconf <- ggplot2::ggplot_build(p)

    expect_equal(length(pconf$data), 5)
  })




  test_that("can update indivual plot labels", {
    ctr <- helpers$ctr

    # Change x- and y-labels
    p2 <- ctr %>%
      pmx_update(
        "individual",
        labels = list(x = "Time (days)", y = "Free serum concentration (nmol)")
      ) %>%
      get_plot("individual", which_pages = 1)
    expect_identical(
      list(
        x = p2$labels$x,
        y = p2$labels$y
      ),
      list(x = "Time (days)", y = "Free serum concentration (nmol)")
    )
  })


  test_that("plot title with start.facet", {
    ctr <- helpers$ctr

    # Change x- and y-labels
    p1 <- ctr %>% pmx_plot_iwres_ipred(strat.color = "AGE0", strat.facet = ~STUD)
    p2 <- ctr %>% pmx_plot_iwres_ipred(strat.color = "AGE0", strat.facet = SEX ~ STUD)
    # Custom label still takes priority
    p3 <- pmx_plot_iwres_ipred(
      ctr,
      strat.color = "AGE0",
      strat.facet = SEX ~ STUD,
      labels = list(title = "CUSTOM_A vs CUSTOM_B by CUSTOM_OVERRIDE")
    )

    expect_identical(p1$labels$title, "IWRES vs IPRED by STUD")
    expect_identical(p2$labels$title, "IWRES vs IPRED by SEX and STUD")
    expect_identical(p3[["labels"]][["title"]], "CUSTOM_A vs CUSTOM_B by CUSTOM_OVERRIDE")
  })
}

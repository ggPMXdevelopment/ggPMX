test_that("individual draft arguments override settings without changing the controller", {
  draft_labels <- function(plot) {
    layers <- ggplot2::ggplot_build(plot)$data
    sum(vapply(
      layers,
      function(layer) {
        sum(layer$label == "DRAFT", na.rm = TRUE)
      },
      numeric(1)
    ))
  }

  for (controller_draft in c(TRUE, FALSE)) {
    ctr <- test_controller(
      "INDIVIDUAL",
      settings = pmx_settings(is.draft = controller_draft)
    )
    individual_plot <- function(...) {
      pmx_plot_individual(
        ctr,
        filter = ID == 1,
        facets = list(nrow = 1, ncol = 1),
        ...
      )
    }
    original_settings <- ctr$settings
    original_labels <- draft_labels(individual_plot())

    expect_equal(original_labels > 0, controller_draft)
    expect_equal(draft_labels(individual_plot(is.draft = FALSE)), 0)
    expect_gt(draft_labels(individual_plot(is.draft = TRUE)), 0)
    expect_identical(ctr$settings, original_settings)
    expect_equal(draft_labels(individual_plot()), original_labels)
  }
})

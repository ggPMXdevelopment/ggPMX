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
    ctr <- theophylline(settings = pmx_settings(is.draft = controller_draft))
    original_settings <- ctr$settings
    original_labels <- draft_labels(pmx_plot_individual(ctr))

    expect_equal(original_labels > 0, controller_draft)
    expect_equal(draft_labels(pmx_plot_individual(ctr, is.draft = FALSE)), 0)
    expect_gt(draft_labels(pmx_plot_individual(ctr, is.draft = TRUE)), 0)
    expect_identical(ctr$settings, original_settings)
    expect_equal(draft_labels(pmx_plot_individual(ctr)), original_labels)
  }
})

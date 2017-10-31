context("Test defaults")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("all plots has not null defaults", {
  ctr <- pmxClassHelpers$ctr
  pmx_plots <- ctr %>% plot_names()
  plots. <- setdiff(pmx_plots, c("indiv", "eta_cats", "eta_conts"))
  res <- lapply(plots., get_plot_defaults)
  expect_true(all(vapply(res, length, 0) > 0))
})

context("Test defaults")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("all plots has not null defaults", {
  ctr <- pmxClassHelpers$ctr
  pmx_plots <- ctr %>% plot_names()
  plots. <- setdiff(pmx_plots, c("individual", "eta_cats", "eta_conts"))
  res <- lapply(plots., get_plot_defaults)
  expect_true(all(vapply(res, length, 0) > 0))
})

test_that("plot_pmx_xx and get plot have same defaults", {
  ctr <- pmxClassHelpers$ctr
  pp <- ctr %>% plots()
  Map(function(pname, fname) {
    p1 <- do.call(fname, list(ctr = ctr))
    p2 <- if (pname == "individual") {
      ctr %>% get_plot(pname, 1)
    } else {
      ctr %>% get_plot(pname)
    }
    d1 <- unlist(as.list(p1$plot_env))
    d2 <- unlist(as.list(p2$plot_env))
    if (!fname %in% c("pmx_plot_eta_matrix")) {
      expect_length(setdiff(d1, d2), 0)
    }
  }, pp$plot_name, pp$plot_function)
})

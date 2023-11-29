if (helper_skip()) {
  context("Test defaults")
  pmxClassHelpers <- test_pmxClass_helpers()
  # pmx_nlmixr()

  test_that("pmx_plot_xx and get plot have same defaults", {
    ctr <- pmxClassHelpers$ctr
    pp <- ctr %>% plots()
    Map(function(pname, fname) {
      p1 <- do.call(fname, list(ctr = ctr))
      p2 <- if (pname == "individual") {
        ctr %>% get_plot(pname, 1)
      } else {
        ctr %>% get_plot(pname)
      }
      p1$plot_env$mm <- NULL
      p2$plot_env$mm <- NULL
      d1 <- unlist(as.list(p1$plot_env))
      d2 <- unlist(as.list(p2$plot_env))
      if (!fname %in% c("pmx_plot_eta_matrix")) {
        expect_true(length(setdiff(d1, d2)) < 2)
      }
    }, pp$plot_name, pp$plot_function)
  })

}

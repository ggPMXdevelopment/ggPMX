context("Test all plots")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("We can call all pmx_plot_xx with success", {
  ctr <- pmxClassHelpers$ctr
  pmx_plots <- ctr %>% plot_names()
  pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)

  res <- lapply(
    pmx_function_plots,
    function(fun) {
      is_function <- exists(fun, where = "package:ggPMX", mode = "function")
      if (is_function) {
        do.call(fun, list(ctr = ctr))
      } else {
        if (fun == "pmx_plot_indiv") {
          ctr %>% pmx_plot_individual(1)
        }
      }
    }
  )
  expect_true(all(vapply(res, function(x) inherits(x, "gg") || is.null(x), TRUE)))
})

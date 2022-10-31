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

test_that("We can call all pmx_plot_xx with title with success", {
  ctr <- theophylline()
  pmx_plots <- c(
    "abs_iwres_ipred", "iwres_ipred", "iwres_time", "iwres_dens", "vpc",
    "npde_time", "npde_pred", "dv_pred", "dv_ipred", "individual", "eta_hist",
    "eta_box", "eta_cats", "eta_conts"
  )
  
  pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
  
  res <- lapply(
    pmx_function_plots,
    function(fun) {
      is_function <- exists(fun, where="package:ggPMX", mode="function")
      if (is_function) {
        do.call(fun, list(ctr = ctr, is.title = TRUE))
      }
    }
  )

  expect_true(all(vapply(res, function(x) x$labels$title != "", TRUE)))
  pmx_plots <- c("iwres_qq", "npde_qq", "eta_qq")
  pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)

  res <- lapply(
    pmx_function_plots,
    function(fun) {
      is_function <- exists(fun, where = "package:ggPMX", mode = "function")
      if (is_function) {
        do.call(fun, list(ctr=ctr, is.title=TRUE))
      }
    }
  )
  
  expect_true(all(vapply(res, function(x) x[["labels"]][["title"]] == "", TRUE)))
  p <- ctr %>% pmx_plot_eta_matrix(is.title=TRUE)
  expect_true(p[["title"]] != "")
})

test_that("We can call all pmx_plot_xx without title with success", {
  ctr <- theophylline()
  pmx_plots <- c("abs_iwres_ipred", "iwres_ipred", "iwres_time", "iwres_dens",
    "iwres_qq", "npde_time", "npde_pred", "vpc", "npde_qq", "dv_pred",
    "dv_ipred", "individual", "eta_hist", "eta_box", "eta_cats", "eta_conts",
    "eta_qq"
  )

  pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
  
  res <- lapply(
    pmx_function_plots,
    function(fun) {
      is_function <- exists(fun, where="package:ggPMX", mode="function")
      if (is_function) {
        do.call(fun, list(ctr = ctr, is.title = FALSE))
      }
    }
  )

  expect_true(all(vapply(res, function(x) x[["labels"]][["title"]] == "", TRUE)))
  p <- ctr %>% pmx_plot_eta_matrix(is.title=FALSE)
  expect_true(p[["title"]] == "")
})

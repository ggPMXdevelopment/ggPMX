test_that("eta QQ uses distinct conditional modes and preserves stratification", {
  ctr <- test_controller("ETA_QQ")
  eta <- get_data(ctr, "eta")
  mode <- unique(eta[, c("ID", "EFFECT", "VALUE", "SEX"), with = FALSE])
  mode$FUN <- "mode"
  mean <- data.table::copy(mode)
  mean$FUN <- "mean"
  mean$VALUE <- mean$VALUE + 10
  set_data(ctr, eta = data.table::rbindlist(list(mode, mode, mean)))
  plot <- pmx_plot_eta_qq(ctr, strat.facet = ~SEX)
  built <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(nrow(built), nrow(mode))
  expect_equal(sort(built$y), sort(mode$VALUE))
  expect_contains(names(plot$data), "SEX")
  mode$FUN <- NULL
  set_data(ctr, eta = data.table::rbindlist(list(mode, mode)))
  expect_equal(
    nrow(ggplot2::ggplot_build(pmx_plot_eta_qq(ctr))$data[[1]]),
    nrow(mode)
  )
})

test_that("eta matrix keeps negative extremes in lower panels", {
  ctr <- test_controller("ETA_MATRIX")
  eta <- get_data(ctr, "eta")
  eta$VALUE[eta$ID == eta$ID[1]] <- -10
  set_data(ctr, eta = eta)
  matrix <- pmx_plot_eta_matrix(ctr, is.smooth = FALSE)
  for (i in seq_len(3)) {
    for (j in seq_len(3)) {
      if (i <= j) {
        next
      }
      panel <- matrix[i + 1L, j]
      point <- which(vapply(
        panel$layers,
        function(x) inherits(x$geom, "GeomPoint"),
        logical(1)
      ))
      built <- ggplot2::ggplot_build(panel)$data[[point]]
      expect_equal(sort(built$x), sort(panel$data[[matrix$xAxisLabels[j]]]))
      expect_equal(
        sort(built$y),
        sort(panel$data[[matrix$yAxisLabels[i + 1L]]])
      )
      expect_equal(
        sum(is.finite(built$x) & is.finite(built$y)),
        nrow(panel$data)
      )
    }
  }
})

test_that("parameter formatting allows missing uncertainty without labelling it fixed", {
  ctr <- test_controller()
  estimates <- get_data(ctr, "estimates")
  for (columns in list(c("PARAM", "VALUE"), setdiff(names(estimates), "RSE"))) {
    reduced <- estimates[, columns, with = FALSE]
    set_data(ctr, estimates = reduced)
    result <- param_table(ctr)
    expect_s3_class(result, "knitr_kable")
    expect_equal(any(grepl("fixed", result, fixed = TRUE)), FALSE)
    expect_equal(param_table(ctr, return_table = TRUE), reduced)
  }
  estimates$RSE[1:3] <- c(NA_real_, Inf, 12)
  set_data(ctr, estimates = estimates)
  result <- param_table(ctr)
  expect_equal(any(grepl("fixed", result, fixed = TRUE)), TRUE)
  expect_equal(any(grepl("12%", result, fixed = TRUE)), TRUE)
})

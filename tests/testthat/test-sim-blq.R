test_that("residual-only normalization preserves the response", {
  data <- data.frame(DV = c(1, 2), iwRes_mode_simBlq = c(0.1, 0.2))
  result <- normalize_sim_blq(data, list())
  expect_equal(result$DV, data$DV)
  expect_equal(result$IWRES, data$iwRes_mode_simBlq)
})

test_that("an observed Y column does not replace DV", {
  data <- data.frame(DV = c(1, 2), Y = c(5, 6))
  expect_equal(normalize_sim_blq(data, list()), data)
})

test_that("simulated responses support both cases and all suffixes", {
  for (variable in c("Y", "y")) {
    for (suffix in c("_mode_simBlq", "_simBlq", "_simBlq_mode")) {
      data <- data.frame(DV = c(1, 2))
      data[[paste0(variable, suffix)]] <- c(3, 4)
      result <- normalize_sim_blq(data, list())
      expect_equal(result$Y, c(3, 4))
      expect_equal(result$DV, c(3, 4))
    }
  }
})

test_that("all configuration sections support residual and response mapping", {
  for (section in c("sim_blq_npde_iwres", "sim_blq_y", "sim_blq")) {
    data <- data.frame(npde_custom = c(0.1, 0.2), y_custom = c(3, 4))
    config <- setNames(
      list(list(names = as.list(setNames(names(data), names(data))))),
      section
    )
    result <- normalize_sim_blq(data, config)
    expect_equal(result$NPDE, data$npde_custom)
    expect_equal(result$Y, data$y_custom)
    expect_equal(result$DV, data$y_custom)
  }
})

test_that("unrelated configured names do not overwrite responses", {
  data <- data.frame(DV = c(1, 2), study = c(3, 4))
  config <- list(sim_blq = list(names = list(study = "study")))
  expect_equal(normalize_sim_blq(data, config), data)
})

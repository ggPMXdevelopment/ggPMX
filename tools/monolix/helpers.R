check_demo_controller <- function(ctr, reference) {
  testthat::expect_s3_class(ctr, "pmxClass")
  predictions <- ggPMX::get_data(ctr, "predictions")
  input <- ggPMX::get_data(ctr, "input")
  estimates <- ggPMX::param_table(ctr, return_table = TRUE)
  testthat::expect_gt(nrow(predictions), 0L)
  testthat::expect_contains(names(predictions), c("ID", "TIME", "DV", "PRED", "IPRED", "IWRES"))
  testthat::expect_contains(names(estimates), c("PARAM", "VALUE"))
  if (!all(c("ID", "TIME", "DV", "PRED", "IPRED", "IWRES") %in% names(predictions))) {
    stop("Prediction columns are incomplete")
  }
  testthat::expect_setequal(as.character(unique(predictions$ID)), as.character(unique(input$ID)))
  testthat::expect_equal(sum(is.na(predictions$ID)), 0L)
  for (column in c("TIME", "DV", "PRED", "IPRED", "IWRES")) {
    testthat::expect_equal(is.numeric(predictions[[column]]), TRUE)
    testthat::expect_equal(sum(!is.finite(predictions[[column]])), 0L, info = column)
  }
  testthat::expect_gt(stats::sd(predictions$PRED), 0)
  testthat::expect_gt(nrow(estimates), 0L)
  testthat::expect_equal(anyDuplicated(estimates$PARAM), 0L)
  testthat::expect_setequal(trimws(estimates$PARAM), reference$parameter)
  matched <- match(trimws(estimates$PARAM), reference$parameter)
  testthat::expect_equal(estimates$VALUE, reference$value[matched], tolerance = 1e-8)
  invisible(list(predictions = predictions, estimates = estimates))
}

check_demo_plot <- function(plot, predictions, x, y, filename) {
  testthat::expect_s3_class(plot, "ggplot")
  built <- ggplot2::ggplot_build(plot)
  points <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  testthat::expect_length(points, 1L)
  if (length(points) != 1L) stop("Expected one observation point layer")
  actual <- built$data[[points]]
  testthat::expect_equal(nrow(actual), nrow(predictions))
  observed <- data.frame(x = actual$x, y = actual$y)
  expected <- data.frame(x = predictions[[x]], y = predictions[[y]])
  observed <- observed[order(observed$x, observed$y), ]
  expected <- expected[order(expected$x, expected$y), ]
  rownames(observed) <- rownames(expected) <- NULL
  testthat::expect_equal(observed, expected, tolerance = 1e-8)
  ggplot2::ggsave(filename, plot = plot, width = 7, height = 5, units = "in")
  testthat::expect_gt(file.info(filename)$size, 0)
}

copy_demo <- function(project, destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  parent <- dirname(project)
  result_name <- tools::file_path_sans_ext(basename(project))
  paths <- file.path(parent, c(basename(project), "data", result_name))
  if (!all(file.exists(paths))) stop("Demo requires its project, data, and fitted results: ", project)
  if (!all(file.copy(paths, destination, recursive = TRUE))) stop("Could not copy demo")
  file.path(destination, basename(project))
}

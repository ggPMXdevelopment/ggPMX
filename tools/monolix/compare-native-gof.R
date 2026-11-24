compare_native_gof <- function(ctr, native, output) {
  coordinate_difference <- function(actual, expected) {
    if (nrow(actual) != nrow(expected)) {
      return(Inf)
    }
    # Native precision can reorder ties introduced by rounded text exports.
    remaining <- seq_len(nrow(expected))
    differences <- numeric(nrow(actual))
    for (i in seq_len(nrow(actual))) {
      distance <- pmax(
        abs(expected$x[remaining] - actual$x[i]),
        abs(expected$y[remaining] - actual$y[i])
      )
      nearest <- which.min(distance)
      differences[i] <- distance[nearest]
      remaining <- remaining[-nearest]
    }
    max(differences)
  }
  paired <- function(data) {
    data <- as.data.frame(data)[c("x", "y")]
    data <- data[order(data$x, data$y), ]
    rownames(data) <- NULL
    data
  }
  results <- lapply(
    c(
      "dv_pred",
      "dv_ipred",
      "iwres_time",
      "iwres_ipred",
      "npde_time",
      "npde_pred"
    ),
    function(name) {
      plot <- ggPMX::pmx_plot(ctr, name, is.smooth = FALSE)
      point <- which(vapply(
        plot$layers,
        function(x) inherits(x$geom, "GeomPoint"),
        logical(1)
      ))
      actual <- paired(ggplot2::ggplot_build(plot)$data[[point]])
      expected <- paired(read.csv(file.path(
        native,
        paste0(name, "-layer-1.csv")
      )))
      difference <- coordinate_difference(actual, expected)
      data.frame(
        check = name,
        points = nrow(actual),
        max_absolute_difference = difference,
        pass = difference < 1e-4
      )
    }
  )
  eta <- ggPMX::get_data(ctr, "eta")
  mode <- unique(as.data.frame(eta)[
    eta$FUN == "mode",
    c("ID", "EFFECT", "VALUE")
  ])
  reference <- readRDS(file.path(native, "random-effects.rds"))$conditionalMode
  for (effect in unique(mode$EFFECT)) {
    actual <- mode[mode$EFFECT == effect, ]
    expected <- reference[[paste0("eta_", effect)]][match(
      actual$ID,
      reference$id
    )]
    difference <- max(abs(actual$VALUE - expected))
    results[[length(results) + 1L]] <- data.frame(
      check = paste0("eta_mode_", effect),
      points = nrow(actual),
      max_absolute_difference = difference,
      pass = difference < 1e-4
    )
  }
  matrix <- ggPMX::pmx_plot_eta_matrix(ctr, is.smooth = FALSE)
  panel <- matrix[
    match("V", matrix$yAxisLabels),
    match("Cl", matrix$xAxisLabels)
  ]
  point <- which(vapply(
    panel$layers,
    function(x) inherits(x$geom, "GeomPoint"),
    logical(1)
  ))
  actual <- paired(ggplot2::ggplot_build(panel)$data[[point]])
  expected <- paired(read.csv(file.path(native, "eta_V_Cl-layer-1.csv")))
  difference <- coordinate_difference(actual, expected)
  results[[length(results) + 1L]] <- data.frame(
    check = "eta_matrix_V_Cl",
    points = nrow(actual),
    max_absolute_difference = difference,
    pass = difference < 1e-4
  )
  result <- do.call(rbind, results)
  write.csv(result, output, row.names = FALSE)
  result
}

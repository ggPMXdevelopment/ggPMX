local({
  if (identical(tolower(Sys.getenv("CI")), "true")) {
    stop("Local-only GOF export")
  }
  library(lixoftConnectors)
  library(ggplot2)
  project <- Sys.getenv("GGPMX_NATIVE_PROJECT")
  output <- Sys.getenv("GGPMX_NATIVE_OUTPUT")
  observation <- Sys.getenv("GGPMX_NATIVE_OBSERVATION", "CONC")
  smoothing <- Sys.getenv("GGPMX_NATIVE_SMOOTHING", "off")
  stopifnot(smoothing %in% c("off", "default", "on"))
  stopifnot(file.exists(project), nzchar(output), !file.exists(output))
  dir.create(output, recursive = TRUE)
  initializeLixoftConnectors(
    software = "monolix",
    path = Sys.getenv("EBROOTMONOLIX"),
    force = TRUE
  )
  loadProject(project)
  save_plot <- function(plot, name) {
    ggsave(
      file.path(output, paste0(name, ".png")),
      plot = plot,
      width = 8,
      height = 6,
      dpi = 120
    )
    if (inherits(plot, "ggplot")) {
      write.csv(
        data.frame(
          geom = vapply(
            plot$layers,
            function(layer) class(layer$geom)[1],
            character(1)
          ),
          stat = vapply(
            plot$layers,
            function(layer) class(layer$stat)[1],
            character(1)
          )
        ),
        file.path(output, paste0(name, "-layers.csv")),
        row.names = FALSE
      )
      layers <- ggplot_build(plot)$data
      for (i in seq_along(layers)) {
        write.csv(
          layers[[i]],
          file.path(output, paste0(name, "-layer-", i, ".csv")),
          row.names = FALSE
        )
      }
    }
  }
  settings <- list(indivEstimate = "mode")
  if (smoothing == "off") {
    settings$spline <- FALSE
  }
  if (smoothing == "on") {
    settings$spline <- TRUE
  }
  save_plot(
    plotObservationsVsPredictions(
      obsName = observation,
      predictions = "pop",
      settings = settings
    ),
    "dv_pred"
  )
  save_plot(
    plotObservationsVsPredictions(
      obsName = observation,
      predictions = "indiv",
      settings = settings
    ),
    "dv_ipred"
  )
  for (residual in c("indiv", "npde")) {
    for (axis in c("time", "prediction")) {
      name <- paste0(
        if (residual == "indiv") "iwres" else "npde",
        "_",
        if (axis == "time") {
          "time"
        } else if (residual == "indiv") {
          "ipred"
        } else {
          "pred"
        }
      )
      save_plot(
        plotResidualsScatterPlot(
          obsName = observation,
          residuals = residual,
          xaxis = axis,
          settings = settings
        ),
        name
      )
    }
  }
  save_plot(
    plotRandomEffectsCorrelation(
      parametersRows = "V",
      parametersColumns = "Cl",
      settings = c(
        settings,
        if (smoothing %in% c("off", "on")) list(regressionLine = FALSE)
      )
    ),
    "eta_V_Cl"
  )
  saveRDS(getEstimatedRandomEffects(), file.path(output, "random-effects.rds"))
  if (identical(Sys.getenv("GGPMX_NATIVE_EXPORT_VPC", "true"), "true")) {
    computeChartsData(
      plot = "vpc",
      output = observation,
      exportVPCSimulations = TRUE
    )
  }
  saveRDS(
    getEstimatedPopulationParameters(),
    file.path(output, "population-parameters.rds")
  )
  writeLines(
    capture.output(sessionInfo()),
    file.path(output, "session-info.txt")
  )
})

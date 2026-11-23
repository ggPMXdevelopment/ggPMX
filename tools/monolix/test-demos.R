library(ggPMX)
library(ggplot2)
library(testthat)

source(file.path(Sys.getenv("GGPMX_HARNESS"), "helpers.R"))
manifest <- read.csv(file.path(Sys.getenv("GGPMX_HARNESS"), "demos.csv"), colClasses = "character")
output <- Sys.getenv("GGPMX_MONOLIX_OUTPUT")
demo_root <- Sys.getenv("GGPMX_MONOLIX_DEMOS")
regenerate <- identical(Sys.getenv("GGPMX_MONOLIX_MODE"), "regenerate")

for (i in seq_len(nrow(manifest))) {
  local({
    demo <- manifest[i, ]
    test_that(paste("Monolix demo:", demo$name), {
      destination <- file.path(output, demo$name)
      warnings <- character()
      on.exit(writeLines(unique(warnings), file.path(destination, "warnings.txt")))
      withCallingHandlers({
        project <- copy_demo(file.path(demo_root, demo$project), destination)
        results <- file.path(destination, tools::file_path_sans_ext(basename(project)))
        charts <- file.path(results, "ChartsData")
        if (regenerate) {
          if (dir.exists(charts) && !file.rename(charts, paste0(charts, ".original"))) {
            stop("Could not preserve original chart data")
          }
          lixoftConnectors::loadProject(project)
          lixoftConnectors::computeChartsData()
        }
        expect_gt(length(list.files(charts, pattern = "\\.txt$", recursive = TRUE)), 0L)
        args <- list(file_name = project)
        if (!is.na(demo$endpoint) && nzchar(demo$endpoint)) {
          args$endpoint <- pmx_endpoint(code = demo$endpoint, file.code = demo$endpoint)
        }
        ctr <- do.call(pmx_mlxtran, args)
        reference <- read.csv(file.path(results, "populationParameters.txt"))
        checked <- check_demo_controller(ctr, reference)
        write.csv(checked$estimates, file.path(destination, "parameters.csv"), row.names = FALSE)
        write.csv(checked$predictions, file.path(destination, "predictions.csv"), row.names = FALSE)
        saveRDS(ctr, file.path(destination, "controller.rds"))
        check_demo_plot(
          pmx_plot_dv_pred(ctr, is.smooth = FALSE), checked$predictions,
          "PRED", "DV", file.path(destination, "dv-pred.png")
        )
        check_demo_plot(
          pmx_plot_iwres_time(ctr, is.smooth = FALSE), checked$predictions,
          "TIME", "IWRES", file.path(destination, "iwres-time.png")
        )
      }, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    })
  })
}

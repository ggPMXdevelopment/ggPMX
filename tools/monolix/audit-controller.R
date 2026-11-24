audit_monolix_controller <- function(
  project,
  output,
  simulations = NULL,
  endpoint = NULL
) {
  if (
    identical(tolower(Sys.getenv("CI")), "true") ||
      nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
  ) {
    stop("This diagnostic audit is local-only")
  }
  if (file.exists(output)) {
    stop("Choose a new output directory")
  }
  dir.create(output, recursive = TRUE)
  arguments <- list(file_name = project)
  if (!is.null(endpoint)) {
    arguments$endpoint <- endpoint
  }
  ctr <- do.call(ggPMX::pmx_mlxtran, arguments)
  saveRDS(ctr, file.path(output, "controller.rds"))
  rows <- list()
  record <- function(name, code) {
    warnings <- messages <- character()
    error <- ""
    tryCatch(
      withCallingHandlers(
        force(code),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          messages <<- c(messages, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) error <<- conditionMessage(e)
    )
    rows[[length(rows) + 1L]] <<- data.frame(
      check = name,
      status = if (nzchar(error)) "FAIL" else "PASS",
      error = error,
      warnings = paste(unique(warnings), collapse = " | "),
      messages = paste(unique(messages), collapse = " | ")
    )
    invisible(NULL)
  }
  render <- function(result, name) {
    plots <- if (inherits(result, "ggplot") || inherits(result, "ggmatrix")) {
      list(result)
    } else {
      result
    }
    stopifnot(length(plots) > 0L)
    for (i in seq_along(plots)) {
      plot <- plots[[i]]
      stopifnot(inherits(plot, "ggplot") || inherits(plot, "ggmatrix"))
      if (inherits(plot, "ggplot")) {
        ggplot2::ggplot_build(plot)
      }
      ggplot2::ggsave(
        file.path(output, paste0(name, "-", i, ".png")),
        plot = plot,
        width = 11,
        height = 8,
        dpi = 120
      )
    }
  }
  for (name in ggPMX::plot_names(ctr)) {
    record(paste0("plot:", name), {
      result <- if (name == "individual") {
        ggPMX::pmx_plot_individual(ctr, which_pages = "all", print = FALSE)
      } else {
        ggPMX::pmx_plot(ctr, name)
      }
      render(result, name)
    })
  }
  predictions <- ggPMX::get_data(ctr, "predictions")
  input <- ggPMX::get_data(ctr, "input")
  eta <- ggPMX::get_data(ctr, "eta")
  parsed <- ggPMX::parse_mlxtran(project)
  charts <- file.path(parsed$directory, "ChartsData")
  observation <- if (is.null(ctr$endpoint)) {
    ctr$dv
  } else {
    paste0("y", ctr$endpoint$file.code)
  }
  category <- ggPMX::get_cats(ctr)[1]
  record("data:Monolix observation and prediction equality", {
    reference <- data.table::fread(file.path(
      charts,
      "ObservationsVsPredictions",
      paste0(observation, "_obsVsPred.txt")
    ))
    stopifnot(all(
      c("ID", "time", observation, "popPred", "indivPredMode") %in%
        names(reference)
    ))
    key <- paste(reference$ID, round(reference$time, 4))
    matched <- match(paste(predictions$ID, predictions$TIME), key)
    stopifnot(!anyNA(matched), nrow(predictions) == nrow(reference))
    for (pair in list(
      c("DV", observation),
      c("PRED", "popPred"),
      c("IPRED", "indivPredMode")
    )) {
      stopifnot(isTRUE(all.equal(
        predictions[[pair[1]]],
        reference[[pair[2]]][matched],
        tolerance = 1e-8
      )))
    }
  })
  record("data:Monolix residual equality", {
    reference <- data.table::fread(file.path(
      charts,
      "ScatterPlotOfTheResiduals",
      paste0(observation, "_residuals.txt")
    ))
    matched <- match(
      paste(predictions$ID, predictions$TIME),
      paste(reference$ID, round(reference$time, 4))
    )
    stopifnot(!anyNA(matched))
    for (pair in list(c("IWRES", "iwRes_mode"), c("NPDE", "npde"))) {
      stopifnot(isTRUE(all.equal(
        predictions[[pair[1]]],
        reference[[pair[2]]][matched],
        tolerance = 1e-8
      )))
    }
  })
  scatter <- list(
    abs_iwres_ipred = c("IPRED", "IWRES"),
    abs_iwres_time = c("TIME", "IWRES"),
    iwres_ipred = c("IPRED", "IWRES"),
    iwres_time = c("TIME", "IWRES"),
    npde_time = c("TIME", "NPDE"),
    npde_pred = c("PRED", "NPDE"),
    dv_pred = c("PRED", "DV"),
    dv_ipred = c("IPRED", "DV")
  )
  for (name in names(scatter)) {
    record(paste0("coordinates:", name), {
      plot <- ggPMX::pmx_plot(ctr, name)
      point <- which(vapply(
        plot$layers,
        function(x) inherits(x$geom, "GeomPoint"),
        logical(1)
      ))
      stopifnot(length(point) == 1L)
      actual <- ggplot2::ggplot_build(plot)$data[[point]]
      expected <- data.frame(
        x = predictions[[scatter[[name]][1]]],
        y = predictions[[scatter[[name]][2]]]
      )
      if (startsWith(name, "abs_")) {
        expected$y <- abs(expected$y)
      }
      actual <- as.data.frame(actual)[c("x", "y")]
      actual <- actual[order(actual$x, actual$y), ]
      expected <- expected[order(expected$x, expected$y), ]
      rownames(actual) <- rownames(expected) <- NULL
      stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-8)))
    })
  }
  record("data:eta QQ has one value per subject and effect", {
    plot <- ggPMX::pmx_plot(ctr, "eta_qq")
    points <- ggplot2::ggplot_build(plot)$data[[1]]
    expected <- length(unique(eta$ID)) * length(unique(eta$EFFECT))
    if (nrow(points) != expected) {
      stop("Expected ", expected, " subject/effect points, got ", nrow(points))
    }
  })
  record("data:eta matrix retains every subject in scatter panels", {
    matrix <- ggPMX::pmx_plot(ctr, "eta_matrix")
    effects <- length(unique(eta$EFFECT))
    for (i in seq_len(effects)) {
      for (j in seq_len(effects)) {
        if (i <= j) {
          next
        }
        panel <- matrix[i + 1L, j]
        point <- which(vapply(
          panel$layers,
          function(x) inherits(x$geom, "GeomPoint"),
          logical(1)
        ))
        built <- suppressWarnings(ggplot2::ggplot_build(panel))$data[[point]]
        visible <- sum(is.finite(built$x) & is.finite(built$y))
        if (visible != nrow(panel$data)) {
          stop(
            "Scatter panel retains ",
            visible,
            " of ",
            nrow(panel$data),
            " subjects"
          )
        }
      }
    }
  })
  record("table:parameter estimates and shrinkage", {
    write.csv(
      ggPMX::param_table(ctr, return_table = TRUE),
      file.path(output, "parameters.csv"),
      row.names = FALSE
    )
    writeLines(
      as.character(ggPMX::param_table(ctr)),
      file.path(output, "parameters.html")
    )
    shrink <- ggPMX::pmx_comp_shrink(ctr, fun = "var")
    stopifnot(all(is.finite(shrink$SHRINK)))
    write.csv(shrink, file.path(output, "shrinkage.csv"), row.names = FALSE)
  })
  record("plot:categorical stratification", {
    render(
      ggPMX::pmx_plot_cats(ctr, "dv_pred", print = FALSE),
      "stratified-dv-pred"
    )
  })
  record("plot:filter and color customization", {
    before <- ggPMX::get_data(ctr, "predictions")
    render(
      do.call(
        ggPMX::pmx_plot_dv_pred,
        list(ctr = ctr, filter = "TIME > 5", strat.color = category)
      ),
      "filtered-dv-pred"
    )
    stopifnot(identical(before, ggPMX::get_data(ctr, "predictions")))
  })
  record("plot:SAEM convergence with explicit modern config", {
    cfg <- ggPMX::load_config("standing", sys = "mlx18")
    cfg$data$saem <- list(
      label = "SAEM convergence",
      file = "ChartsData/Saem/CvParam.txt",
      reader = "read_mlx_saem_conv"
    )
    extended <- do.call(
      ggPMX::pmx_mlxtran,
      c(arguments, list(config = cfg))
    )
    render(ggPMX::pmx_plot_saem_convergence(extended), "saem_convergence")
  })
  if (!is.null(simulations)) {
    record("plot:VPC with Monolix simulations and explicit bins", {
      sim <- data.table::fread(simulations)
      stopifnot(all(
        c("ID", "time", "rep", paste0("sim_", observation)) %in% names(sim)
      ))
      data.table::setnames(
        sim,
        c("time", paste0("sim_", observation)),
        c("TIME", ctr$dv)
      )
      extended <- do.call(
        ggPMX::pmx_mlxtran,
        c(
          arguments,
          list(
            sim = ggPMX::pmx_sim(data = sim, irun = "rep", idv = "TIME")
          )
        )
      )
      stopifnot(nrow(ggPMX::get_data(extended, "sim")) == nrow(sim))
      render(
        ggPMX::pmx_plot_vpc(
          extended,
          bin = ggPMX::pmx_vpc_bin(style = "kmeans", n = 8)
        ),
        "vpc-binned"
      )
    })
  }
  summary <- do.call(rbind, rows)
  write.csv(summary, file.path(output, "audit.csv"), row.names = FALSE)
  images <- list.files(output, pattern = "\\.png$")
  writeLines(
    c(
      "<!doctype html><meta charset='utf-8'><title>Monolix diagnostic audit</title>",
      paste0(
        "<h1>",
        basename(project),
        " — ",
        observation,
        "</h1><p>See audit.csv for checks and warnings.</p>"
      ),
      unlist(lapply(images, function(file) {
        c(
          paste0("<h2>", file, "</h2>"),
          paste0("<img style='max-width:100%' src='", file, "'>")
        )
      }))
    ),
    file.path(output, "index.html")
  )
  summary
}

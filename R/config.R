
#' This function can be used to define the pmx configuration used in plots. e.g. Monolox/Nonmem
#'
#' @param sys \code{charcarter} system used , monolix,nonmem,...
#' @param inputs \code{charcater} path to the inputs settings file (yaml format)
#' @param plots   \code{charcater} path to the inputs settings file (yaml format)
#' @param ...  extra arguments not used
#'
#' @return \code{pmxConfig} object
#' @export
#' @example inst/examples/pmx_config.R
#' @details
#' To create a controller user can create a pmxConfig object using \cr
#'  - either an input template file \cr
#'  - or a plot template file \cr
#'  - or both. \cr
#' By default the 'standing' configuration will be used.
pmx_config <- function(sys = "mlx", inputs, plots, ...) {
 
  if (missing(inputs)) {
     inputs <-  system.file(package = "ggPMX", "init", "mlx","standing.ipmx")
  }
  if (missing(plots)) {
    plots <-  system.file(package = "ggPMX", "init","standing.ppmx")
  }

  if (!file.exists(inputs)) stop("inputs template file does not exist")
  if (!file.exists(plots)) stop("plots template file does not exist")
  load_config_files(inputs, plots, sys)
}




#' Get List of built-in configurations
#' @param sys can be mlx, by default all configurations will be listed
#' @return names of the config
#' @export
#'
#' @examples
#' pmx_get_configs()
pmx_get_configs <-
  function(sys = "mlx") {
    sys <- tolower(sys)
    template_dir <-
      file.path(system.file(package = "ggPMX"), "templates", sys)
    res <- if (dir.exists(template_dir)) {
      template_path <- list.files(
        template_dir,
        full.names = TRUE,
        recursive = FALSE
      )
      if (length(template_path) == 0) {
        return(NULL)
      }
      template_name <- gsub("[.].*", "", basename(template_path))
      dx <- data.frame(
        sys = sys,
        name = template_name,
        path = template_path,
        stringsAsFactors = FALSE
      )
      class(dx) <- c("configs", "data.frame")
      dx
    }
    res
  }

#' This function can be used to print configuration of the defined object using S3 method.
#' @param x object of class configs
#' @param ... pass additional options (not used presently)
#' @return print result
#' @export
print.configs <- function(x, ...) {
  assert_that(is_configs(x))
  cat(sprintf(
    "There are %i configs for %s system \n",
    nrow(x), unique(x$sys)
  ))
  for (i in seq_len(nrow(x))) {
    cat(sprintf("config %i : name %s \n", i, x[i, "name"]))
  }
}

#' Obtain the data source config
#'
#' @param x the config name.
#' @param sys can be mlx,nm,...
#' @return a list :data configuration object
#' @importFrom  yaml yaml.load_file
#' @export
load_config <- function(x, sys = c("mlx", "nm", "mlx18")) {
  assert_that(is_string(x))
  sys <- match.arg(sys)
  input_dir <-
    file.path(system.file(package = "ggPMX"), "templates", sys)
  plot_dir <-
    file.path(system.file(package = "ggPMX"), "init")
  ifile <- file.path(input_dir, sprintf("%s.ipmx", x))
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", x))
  if (length(ifile) == 0) {
    stop(sprintf("No configuration found for: %s", x))
  }
  if (length(ifile) == 0) {
    stop(sprintf("No configuration found for: %s", x))
  }
  load_config_files(ifile, pfile, sys)
}


load_config_files <- function(ifile, pfile, sys) {
  if (!file.exists(ifile)) {
    return(NULL)
  }
  if (!file.exists(pfile)) {
    return(NULL)
  }
  iconfig <- yaml.load_file(ifile)
  pconfig <- yaml.load_file(pfile)
  config <- list(data = iconfig, plots = pconfig)
  config$sys <- sys
  class(config) <- "pmxConfig"
  config
}


#' S3 method print pmxConfig object
#'
#' @param x pmxConfig object
#' @param ... addtional arguments to pass to print (unused currently)
#'
#' @return invisible object
#' @importFrom knitr kable
#' @export
print.pmxConfig <-
  function(x, ...) {
    data_name <- plot_name <- NULL
    assert_that(is_pmxconfig(x))
    if (exists("data", x)) {
      datas_table <- data.table(
        data_name = names(x$data),
        data_file = sapply(x$data, "[[", "file"),
        data_label = sapply(x$data, "[[", "label")
      )


      ctr <- list(...)$ctr
      if (!is.null(ctr)) {
        datas_table <- rbind(
          datas_table,
          data.table(
            data_name = "input",
            data_file = if (!is.null(ctr$input_file)) basename(ctr$input_file) else "",
            data_label = "modelling input"
          )
        )
      }
      datas_table <- datas_table[ data_name %in% c("input", names(ctr$data))]
      print(kable(datas_table), format = "latex")
    }

    if (exists("plots", x)) {
      plots_table <- data.table(
        plot_name = tolower(names(x$plots)),
        plot_type = sapply(x$plots, "[[", "ptype")
      )
      plot_names <- list(...)$plot_names
      if (!is.null(plot_names)) {
        plots_table <- plots_table[ plot_name %in% plot_names]
      }
      print(kable(plots_table), format = "latex")
    }

    invisible(x)
  }



pmx_warnings <- function(x, warn) {
  assert_that(is_pmxclass(x))

  if (warn %in% names(x$warnings)) {
    message(x$warnings[[warn]])
  }
}

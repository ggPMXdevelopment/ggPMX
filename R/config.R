

#' List configurations
#' @param sys can be mlx, if missed all configurations will be listed
#' @return names of the config
#' @export
#'
#' @examples
#' configs()
configs <-
  function(sys = "mlx"){
    sys <- tolower(sys)
    template_dir <- 
      file.path(system.file(package = "ggPMX"), "templates", sys)
    res <- if(dir.exists(template_dir)){
      template_name <- list.dirs(template_dir, full.names = FALSE, 
                                 recursive = FALSE)
      if(length(template_name) == 0)return(NULL)
      template_path <- list.dirs(template_dir, full.names = TRUE, 
                                 recursive = FALSE)
      dx <- data.frame(
        sys=sys,
        name=template_name,
        path=template_path,
        stringsAsFactors = FALSE
      )
      class(dx) <- c("configs", "data.frame")
      dx
    }
    res
  }

#' S3 method to print configs
#' @param x object of class configs
#' @param ... pass additonal options (not used presently)
#' @return print result
#' @export
print.configs <- function(x, ...){
  assert_that(is_configs(x))
  cat(sprintf("There are %i configs for %s system \n",
              nrow(x), unique(x$sys)))
  for (i in seq_len(nrow(x)))
    cat(sprintf("config %i : name %s \n", i, x[i, "name"]))
}

#' Get data source config
#'
#' @param x the config name.
#' @param sys can be mlx,nm,...
#' @return a list :data configuration object
#' @importFrom  yaml yaml.load_file
#' @export
load_config <- function(x, sys = c("mlx", "nm")){
  assert_that(is_string(x))
  sys <- match.arg(sys)
  configs. <- configs(sys)
  cpath <- configs.[configs.$name == x, "path"]
  ifile <- list.files(cpath, full.names = TRUE, pattern = "ipmx")
  if(length(ifile)==0)
    stop(sprintf("No configuration found for: %s",x))
  iconfig <- yaml.load_file(ifile)
  pfile <- list.files(cpath, full.names = TRUE, pattern = "ppmx")
  pconfig <- yaml.load_file(pfile)
  config <- list(data=iconfig, plots = pconfig)
  config$sys <- sys
  class(config) <- "pmxConfig"
  config
}


#' S3 method print pmxConfig object
#'
#' @param x pmxConfig object
#' @param ... addtional arguments to pass to print (ununsed currently)
#'
#' @return invisible object
#' @importFrom knitr kable
#' @export
print.pmxConfig <-
  function(x, ...){
    assert_that(is_pmxconfig(x))
    if (exists("data", x)) {
      datas_table <- data.table(
        data_name=names(x$data),
        data_file=sapply(x$data,"[[","file"),
        data_label=sapply(x$data,"[[","label"))
                                
      print(kable(datas_table),format = "latex",caption = "List of data sets:")
    }
    
    if (exists("plots", x)) {
      plots_table <- data.table(
        plot_name=tolower(names(x$plots)),
        plot_type=sapply(x$plots,"[[","ptype"))
      
      print(kable(plots_table),format = "latex",caption = "List of plots:")
    }
    
    invisible(x)
  }


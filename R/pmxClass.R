
#' Create a pmx object data Source
#'
#' @param config Can be either :
#' The complete path for the configuration file, the name of configuration within the built-in
#' list of configurations, or a configuration object.
#' @param sys the system name can be MLX/NM/OTHERS
#' @param directory where the files are located. This is an optional parameter by default pmw will look
#' in \code{work_dir} pmx options: \code{getPmxOptions("work_dir")}
#' @return a pmxClass object
#' @export
#' @examples
#'

pmx <-
  function(config, sys, directory){
    if(missing(directory))
      directory <- getPmxOption("work_dir")
    if(is.null(directory))
      stop("Please set a directory argument or set global work_dir option")
    if(!inherits(config,"pmxConfig"))
      config <- load_config(config, sys)
    pmxClass$new(directory, config)
  }

#' Wrapper to pmx constructor
#'
#' @param config the config name
#' @param directory the data directory (working diectory)
#'
#' @return \code{pmxClass} object
#' @export

pmx_mlx <-
  function(config, directory){
    pmx(config, "mlx", directory)
  }


#' Create a new plot  of the desired type
#'
#' @param ctr \code{pmxClass} controller object
#' @param ptype plot type can be:
##' \itemize{
##'  \item{"IND"}{ Individual plot type: \code{\link{individual}} }
##'  \item{"DIS"}{ Distribution plot type : \code{\link{distrib}}}
##'  \item{"RES"}{ Residual plot type :\code{\link{residual}}}
##' }
##' @param pname plot name, if missing it will be created using function aestetics
#' @param ... other plot parameters to configure \code{"pmx_gpar"}.
#'
#' @return invisible ctr object
#' @export

#'
set_plot <- function(ctr, ptype, pname, ...){
  
  conf <-
    switch(ptype,
           IND = individual(...),
           DIS = distrib(...),
           RES = residual(...)
    )
  if(ptype=="DIS" && conf$has.shrink)
    conf$shrink <- ctr$data[["shrink"]]
  
  ctr$add_plot(conf, pname)
  invisible(ctr)
}


#' Get plot object
#'
#' @param ctr  \code{pmxClass} controller object
#' @param nplot characater the plot name
#' @param npage integer or integer vector, set page number in case of multi pages plot
#'
#' @return ggplot object
#' @export

get_plot <- function(ctr, nplot, npage){
  xx <- ctr$get_plot(nplot)
  if(inherits(xx, "list"))
    xx[npage]
  else xx
}



#' Get plot names
#'
#' @param ctr  \code{pmxClass} controller object
#' @return list of plot names
#' @export

plot_names <- function(ctr){
  ctr$plots()
  
}



#' Get plot object
#'
#' @param ctr  \code{pmxClass} controller object
#' @param pname characater the plot name to update
#' @param ... others graphical parametrs given to set the plot
#' @param  pmxgpar a object of class pmx_gpar possibly the output of the
#' \code{\link{pmx_gpar}} function.
#'
#' @return ggplot object
#' @export

update_plot <- function(ctr, pname, ..., pmxgpar = NULL){
  ctr$update_plot(pname, ..., pmxgpar = pmxgpar)
}




# pmxSource (R6 Class) ------------------------------------------------------------
#' @importFrom R6 R6Class
pmxClass <- R6::R6Class(
  "pmxClass",
  
  # Private methods ------------------------------------------------------------
  private = list(
    .data_path = "",
    .plots = list(),
    .plots_configs = list()
  ),
  
  # Public methods -------------------------------------------------------------
  public = list(
    data  = NULL,
    config = NULL,
    initialize = function(data_path, config)
      pmx_initialize(self, private, data_path, config),
    
    print = function(data_path, config, ...)
      pmx_print(self, private, ...),
    
    # Operations ---------------------------------------------------------------
    add_plot = function(x, pname)
      pmx_add_plot(self, private, x, pname),
    
    update_plot = function(pname, ..., pmxgpar = NULL)
      pmx_update_plot(self, private, pname, ..., pmxgpar = pmxgpar),
    
    remove_plot = function(pname, ...)
      pmx_remove_plot(self, private, pname, ...),
    
    get_config = function(pname)
      pmx_get_config(self, private, pname),
    
    get_plot = function(pname)
      pmx_get_plot(self, private, pname),
    
    plots = function()
      pmx_plots(self, private),
    
    post_load = function()
      pmx_post_load(self, private)
  )
)

pmx_initialize <- function(self, private, data_path, config) {
  if (missing(data_path) || missing(data_path))
    stop("Expecting source path(directory ) and a config path", 
         call. = FALSE)
  private$.data_path <- data_path
  self$config <- config
  self$data <- load_source(sys=config$sys, private$.data_path, 
                           self$config$data)
  self$post_load()
  
  for ( nn in names(self$config$plots)){
    x <- self$config$plots[[nn]]
    x$pname <- tolower(nn)
    do.call(set_plot, c(ctr = self, x))
  }
  
}

pmx_print <- function(self, private, ...){
  cat("\npmx object:\n")
  cat("data path: ", private$.data_path , "\n")
  print(self$config, ...)
  
}

pmx_add_plot <- function(self, private, x, pname){
  if(missing(pname))
    pname <- paste(x$aess, collapse="_")
  pname <- tolower(pname)
  private$.plots_configs[[pname]] <- x
  vv <- vapply(self$data, function(y){
    all(as.character(x$aess) %in% names(y))
  }, TRUE)
  if(!any(vv)){return(invisible(self))}
  dname <- names(self$data)[vv]
  private$.plots[[pname]] <- plot_pmx(x, dx = self$data[[dname]])
  invisible(self)
}

pmx_update_plot <- function(self, private, pname, ..., pmxgpar){
  # assertthat::assert_that(isnullOrPmxgpar(pmxgpar))
  config <- private$.plots_configs[[pname]]
  old_class <- class(config)
  x <- if(is.null(pmx_gpar)){
    l_left_join(config, ...)
  }else{
    l_left_join(config, pmxgpar)
  }
  class(x$gp) <- class(config$gp)
  self$remove_plot(pname)
  self$add_plot(x, pname)
  
}

pmx_remove_plot <- function(self, private, pname, ...){
  private$.plots_configs[[pname]] <- NULL
  private$.plots[[pname]] <- NULL
  invisible(self)
}

pmx_get_config <- function(self, private, pname){
  pname <- tolower(pname)
  private$.plots_configs[[pname]]
}

pmx_get_plot <- function(self, private, pname){
  pname <- tolower(pname)
  private$.plots[[pname]]
}

pmx_plots <- function(self, private){
  names(private$.plots)
}

pmx_post_load <- function(self, private){
  self$data <- post_load(self$data, self$config$sys, self$config$plots)
  
}

#' Print pmxClass object
#'
#' @param x pmxClass object
#' @param ... additinal arguments to pass to print
#'
#' @return print object to screen
#' @export

print.pmxClass <- function(x, ...){
  x$print(...)
}


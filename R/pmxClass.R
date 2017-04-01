
#' Create a pmx object
#' 
#' Create a pmx object from a data source
#' @param config Can be either :
#' The complete path for the configuration file, the name of configuration within the built-in
#' list of configurations, or a configuration object.
#' @param sys the system name can be MLX/NM/OTHERS
#' @param directory where the files are located. This is an optional parameter by default pmw will look
#' in \code{work_dir} pmx options: \code{getPmxOptions("work_dir")}
#' @param input the input file
#' @param dv the dv parameter
#' @return a pmxClass object
#' @family pmxclass
#' @export
#' @examples
#'

pmx <-
  function(config, sys, directory, input, dv){
    directory <- checkPmxOption(directory, "work_dir")
    input <- checkPmxOption(input, "input")
    dv <- checkPmxOption(dv, "dv")
    if(!inherits(config, "pmxConfig"))
      config <- load_config(config, sys)
    pmxClass$new(directory, input, dv, config)
  }

#' Wrapper to pmx constructor
#'
#' @param config the config name
#' @param directory the data directory (working diectory)
#' @param input the input file
#' @param dv the dv parameter
#'
#' @family pmxclass
#' @return \code{pmxClass} object
#' @export

pmx_mlx <-
  function(config, directory, input, dv){
    pmx(config, "mlx", directory, input, dv)
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
##' @param filter optional filter which will be applied to plotting data
#' @param ... other plot parameters to configure \code{"pmx_gpar"}.
#'
#' @family pmxclass
#' @return invisible ctr object
#' @export

#'
set_plot <- function(ctr, ptype = c("IND", "DIS", "RES"), pname, 
                     filter = NULL, ...){
  assert_that(is_pmxclass(ctr))
  assert_that(is_string_or_null(pname))
  ptype <- match.arg(ptype)
  
  conf <-
    switch(ptype,
           IND = individual(...),
           DIS = distrib(...),
           RES = residual(...)
    )
  if(ptype=="DIS" && conf$has.shrink)
    conf$shrink <- ctr$data[["shrink"]]
  if(!is.null(substitute(filter))){
    filter <- deparse(substitute(filter))
    filter <- local_filter(filter)
    conf[["filter"]] <- filter
  }
  
  ctr[["config"]][["plots"]][[toupper(pname)]] <- 
    c(ptype = ptype, list(...))
  ctr$add_plot(conf, pname)
  invisible(ctr)
}


#' Get plot object
#'
#' @param ctr  \code{pmxClass} controller object
#' @param nplot character the plot name
#' @param npage integer or integer vector, set page number in case of multi pages plot
#'
#' @family pmxclass
#' @return ggplot object
#' @export

get_plot <- function(ctr, nplot, npage = NULL){
  if(is.numeric(npage)){
    npage <- as.integer(npage)
  }
  assert_that(is_pmxclass(ctr))
  assert_that(is_string(nplot))
  assert_that(is_integer_or_null(npage))
  xx <- ctr$get_plot(nplot)
  if(inherits(xx, "list"))
    xx[npage]
  else xx
}



#' Get plot names
#'
#' @param ctr  \code{pmxClass} controller object
#' 
#' @family pmxclass
#' @return list of plot names
#' @export

plot_names <- function(ctr){
  assert_that(is_pmxclass(ctr))
  ctr$plots()
}



#' Update plot object
#'
#' @param ctr  \code{pmxClass} controller object
#' @param pname character the plot name to update
#' @param filter optional filter which will be applied to plotting data
#' @param ... others graphical parameters given to set the plot
#' @param  pmxgpar a object of class pmx_gpar possibly the output of the
#' \code{\link{pmx_gpar}} function.
#'
#' @family pmxclass
#' @return controller object with the plot updated
#' @export

pmx_update <- function(ctr, pname, filter = NULL, ..., pmxgpar = NULL){
  assert_that(is_pmxclass(ctr))
  assert_that(is_string(pname))
  if(!is.null(substitute(filter))){
    filter <- deparse(substitute(filter))
    filter <- local_filter(filter)
  }
  ctr$update_plot(pname, filter = filter, ..., pmxgpar = pmxgpar)
}


#' Get the plot config by name
#'
#' @param ctr the controller object 
#' @param pname the plot name 
#'
#' @family pmxclass
#' @return the config object
#' @export
#'
#' @examples
#' \dontrun{
#' pmxOptions(work_dir = WORK_DIR)
#' ctr <- pmx_mlx(config = "standing")
#' ctr %>% set_plot("IND", pname = "indiv1")
#' get_plot_config("distr1")
#' }
get_plot_config <- function(ctr, pname){
  assert_that(is_pmxclass(ctr))
  ctr$get_config(pname)
}


#' Get controller data set
#'
#' @param ctr the controller object 
#' @param data_set the data set name
#'
#' @family pmxclass
#' @return a data.table of the named data set if available.
#' @export
get_data <- function(ctr, data_set = c("par_est","mod_pred", 
                                      "ind_pred", "finegrid", "shrink")){
  assert_that(is_pmxclass(ctr))
  data_set <- match.arg(data_set)
  ctr[["data"]][[data_set]]
}

# pmxSource (R6 Class) ------------------------------------------------------------
#' @importFrom R6 R6Class
pmxClass <- R6::R6Class(
  "pmxClass",
  
  # Private methods ------------------------------------------------------------
  private = list(
    .data_path = "",
    .input="",
    .plots = list(),
    .plots_configs = list()
  ),
  
  # Public methods -------------------------------------------------------------
  public = list(
    data  = NULL,
    config = NULL,
    input=NULL,
    dv=NULL,
    initialize = function(data_path, input, dv, config)
      pmx_initialize(self, private, data_path, input, dv, config),
    
    print = function(data_path, config, ...)
      pmx_print(self, private, ...),
    
    # Operations ---------------------------------------------------------------
    add_plot = function(x, pname)
      pmx_add_plot(self, private, x, pname),
    
    update_plot = function(pname, filter = NULL, ..., pmxgpar = NULL)
      pmx_update_plot(self, private, pname, filter = filter,
                      ..., pmxgpar = pmxgpar),
    
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

pmx_initialize <- function(self, private, data_path, input, dv, config) {
  if (missing(data_path) || missing(data_path))
    stop("Expecting source path(directory ) and a config path", 
         call. = FALSE)
  private$.data_path <- data_path
  private$.input <- input
  self$config <- config
  self$dv <- dv
  self$input <- read_input(input, self$dv)
  self$data <- load_source(sys=config$sys,  private$.data_path,
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


## TODO change the way how we choose the data
## USE AN EXPLICIT METHOD
## data_set(s) for res,data_set(s) for IND,..
pmx_add_plot <- function(self, private, x, pname){
  if(missing(pname))
    pname <- paste(x$aess, collapse="_")
  pname <- tolower(pname)
  private$.plots_configs[[pname]] <- x
  ptype <- self[["config"]][["plots"]][[toupper(pname)]][["ptype"]]
  dname <- switch(
    ptype,
    RES="mod_pred",
    IND="IND",
    DIS="ind_pred")
  
  private$.plots[[pname]] <- plot_pmx(x, dx = self$data[[dname]])
  invisible(self)
}

pmx_update_plot <- function(self, private, pname, filter, ..., pmxgpar){
  # assertthat::assert_that(isnullOrPmxgpar(pmxgpar))
  config <- private$.plots_configs[[pname]]
  old_class <- class(config)
  x <- if(is.null(pmxgpar)){
    newopts <- list(...)
    hl <- newopts[names(newopts) %in% unique(c(names(config), "shrink"))]
    gpl <- newopts[!names(newopts) %in% unique(c(names(config), "shrink"))]
    hl$gp <- gpl 
    
    l_left_join(config, hl)
  }else{
    l_left_join(config, pmxgpar)
  }
  class(x$gp) <- class(config$gp)
  class(x) <- old_class
  if(!is.null(filter)){
    x[["filter"]] <- filter
  }
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
  self$data <- post_load(self$data, self$input, self$config$sys, 
                         self$config$plots)
  
}

#' Print pmxClass object
#'
#' @param x pmxClass object
#' @param ... additinal arguments to pass to print
#'
#' @family pmxclass functions
#' @return print object to screen
#' @export

print.pmxClass <- function(x, ...){
  x$print(...)
}


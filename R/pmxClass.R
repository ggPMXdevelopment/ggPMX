
#' Create a pmx object
#' 
#' Create a pmx object from a data source
#' @param config Can be either :
#' The complete path for the configuration file, the name of configuration within the built-in
#' list of configurations, or a configuration object.
#' @param sys the system name can be MLX/NM/OTHERS
#' @param directory where the files are located. This is an optional parameter by default pmw will look
#' in \code{work_dir} pmx options: \code{getPmxOptions("work_dir")}
#' @param input \code{character} complete path to the modelling input file
#' @param dv \code{character} the name of measurable variable used in the input modelling file
#' @param dvid \code{character} observation type parameter
#' @param cats \code{character} vector of categorical covariates
#' @param conts \code{character} vector of continuous covariates
#' @param occ \code{character} occasinal covariate variable name
#' @return a pmxClass object
#' @family pmxclass 
#' @export
#' @examples
#'\dontrun{
#'
#'## Example to create the controller object:
#' workdir <- "~/SVN/ggPMX_doc/USER_EXAMPLES/MLX/PK_NO_COVARIATE"
#' WORK_DIR <- file.path(workdir, "RESULTS")
#' input_file <- file.path(workdir, "oral_data.txt")

#' ctr <- pmx(sys="mlx",
#'            config = "standing", 
#'            directory = WORK_DIR,input = input_file,dv="Y")
#'## Better option is to use pmxOptions
#'pmxOptions(work_dir = WORK_DIR, input = input_file, dv = "Y")
#'## Now the latter call becomes
#'ctr <- pmx(sys="mlx", config = "standing")
#'## Or even simpler
#'ctr1 <- pmx_mlx("standing")
#'}

pmx <-
  function(config, sys=c("mlx","nm"), directory, input, dv,dvid,cats,conts,occ,strats,settings){
    directory <- checkPmxOption(directory, "work_dir")
    input <- checkPmxOption(input, "input")
    dv <- checkPmxOption(dv, "dv")
    dvid <- checkPmxOption(dvid, "dvid","DVID")
    cats <- checkPmxOption(cats, "cats","")
    conts <- checkPmxOption(conts, "conts","")
    occ <- checkPmxOption(occ, "occ","")
    strats <- checkPmxOption(strats, "strats","")
    if(!inherits(config, "pmxConfig"))
      config <- load_config(config, sys)
    pmxClass$new(directory, input, dv, config,dvid,cats,conts,occ,strats,settings)
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
  function(config, directory, input, dv,dvid,cats,conts,occ,strats,settings){
    pmx(config, "mlx", directory, input, dv,dvid,cats,conts,occ,strats,settings)
  }


formula_to_text <- function(form){
  if(is.formula(form))
    Reduce(paste, deparse(form))
  else form
  
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
#' @param ... other plot parameters to configure \code{\link{pmx_gpar}}.
#'
#' @family pmxclass
#' @return invisible ctr object
#' @export

#'
set_plot <- function(ctr, ptype = c("IND", "DIS", "RES","ECORREL"), pname, 
                     filter = NULL, strat.color=NULL,strat.facet=NULL,...){
  assert_that(is_pmxclass(ctr))
  ptype <- match.arg(ptype)
  assert_that(is_string_or_null(pname))
  ## assert_that(is_string_or_expression(filter))
  assert_that(is_string_or_null(strat.color)) 
  assert_that(is_string_or_formula_or_null(strat.facet)) 
  
  
  
  conf <-
    switch(ptype,
           IND = individual(...),
           DIS = distrib(...),
           RES = residual(...),
           ECORREL=ecorrel(...)
    )
  if(ptype=="DIS" && conf$has.shrink)
    conf$shrink <- ctr$data[["shrink"]]
  if(!is.null(substitute(filter))){
    filter <- deparse(substitute(filter))
    filter <- local_filter(filter)
  }
  conf[["filter"]] <- filter
  if(!is.null(strat.color)){
    conf[["strat.color"]] <- strat.color
    gp <- conf[["gp"]]
    gp[["labels"]][["legend"]] <- strat.color
    conf[["gp"]] <- gp
    
  }
  if(!is.null(strat.facet)){
    conf[["strat.facet"]] <- strat.facet
    gp <- conf[["gp"]]
    gp[["labels"]][["title"]] <- 
      sprintf("%s by %s",
              gp[["labels"]][["title"]],formula_to_text(strat.facet))
    conf[["gp"]] <- gp
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
#' @examples 
#' \dontrun{
#' library(ggPMX)
#' ctr <- pmx_mlx("standing")
#' ## get all pages or some pages
#' p1 <- ctr %>% get_plot("indiv")
#' p2 <- ctr %>% get_plot("indiv",napge=1)
#' p3 <- ctr %>% get_plot("indiv",napge=c(1,3))
#' ## get distribution plot
#' pdistri <- ctr %>% get_plot("distri")
#' 
#' }

get_plot <- function(ctr, nplot, npage = NULL){
  if(is.numeric(npage)){
    npage <- as.integer(npage)
  }
  assert_that(is_pmxclass(ctr))
  assert_that(is_string(nplot))
  assert_that(is_integer_or_null(npage))
  nplot <- tolower(nplot)
  assert_that(is_valid_plot_name(nplot,plot_names(ctr)))
  xx <- ctr$get_plot(nplot)
  if(is.function(xx))xx(npage)
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
#' @param strat.color optional stratification parameter
#' @param strat.color optional stratification parameter
#' @param ... others graphical parameters given to set the plot
#' @param  pmxgpar a object of class pmx_gpar possibly the output of the
#' \code{\link{pmx_gpar}} function.
#'
#' @family pmxclass
#' @return controller object with the plot updated
#' @export

pmx_update <- function(ctr, pname, filter = NULL,strat.color=NULL,strat.facet=NULL, ..., pmxgpar = NULL){
  assert_that(is_pmxclass(ctr))
  assert_that(is_string(pname))
  if(!is.null(substitute(filter))){
    filter <- deparse(substitute(filter))
    filter <- local_filter(filter)
  }
  assert_that(is_string_or_null(strat.color)) 
  assert_that(is_string_or_formula_or_null(strat.facet)) 
  
  ctr$update_plot(
    pname, filter = filter,strat.color=strat.color,
    strat.facet=strat.facet, ..., pmxgpar = pmxgpar)
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
get_data <- function(ctr, data_set = c("estimates","predictions", 
                                       "eta", "finegrid", "shrink",
                                       "input")){
  assert_that(is_pmxclass(ctr))
  data_set <- match.arg(data_set)
  if(data_set=="input")ctr[["input"]]
  else  ctr[["data"]][[data_set]]
}


#' Get category covariates
#'
#' @param ctr the controller object 
#'
#' @family pmxclass
#' @return a charcater vector 
#' @export
get_cats <- function(ctr){
  assert_that(is_pmxclass(ctr))
  ctr$cats
}


#' Get extra stratification variables
#'
#' @param ctr the controller object 
#'
#' @family pmxclass
#' @return a charcater vector 
#' @export
get_strats <- function(ctr){
  assert_that(is_pmxclass(ctr))
  ctr$strats
}

#' Get covariates variables
#'
#' @param ctr the controller object 
#'
#' @family pmxclass
#' @return a charcater vector 
#' @export
get_covariates <- function(ctr){
  assert_that(is_pmxclass(ctr))
  unique(c(ctr$cats ,ctr$conts))
}



#' Get continuous covariates
#'
#' @param ctr the controller object 
#'
#' @family pmxclass
#' @return a charcater vector 
#' @export
get_conts <- function(ctr){
  assert_that(is_pmxclass(ctr))
  ctr$conts
}


#' Get controller occasional covariates
#'
#' @param ctr the controller object 
#'
#' @family pmxclass
#' @return a charcater vector 
#' @export
get_occ <- function(ctr){
  assert_that(is_pmxclass(ctr))
  ctr$occ
}

# pmxSource (R6 Class) ------------------------------------------------------------
#' @importFrom R6 R6Class
pmxClass <- R6::R6Class(
  "pmxClass",
  
  # Private methods ------------------------------------------------------------
  private = list(
    .data_path = "",
    .input="",
    .covariates =NULL,
    .plots = list(),
    .plots_configs = list()
  ),
  
  # Public methods -------------------------------------------------------------
  public = list(
    data  = NULL,
    config = NULL,
    input=NULL,
    dv=NULL,
    dvid=NULL,cats=NULL,conts=NULL,occ=NULL,
    strats=NULL,
    settings=NULL,
    initialize = function(data_path, input, dv, config, dvid, cats, conts, occ,strats,settings)
      pmx_initialize(self, private, data_path, input, dv, config, dvid, cats, conts, occ,strats,settings),
    
    print = function(data_path, config, ...)
      pmx_print(self, private, ...),
    
    # Operations ---------------------------------------------------------------
    add_plot = function(x, pname)
      pmx_add_plot(self, private, x, pname),
    
    update_plot = function(pname, filter = NULL,strat.facet=NULL,strat.color=NULL,
                           ..., pmxgpar = NULL){
      pmx_update_plot(self, private, pname, filter = filter,
                      strat.color=strat.color, strat.facet = strat.facet,
                      ..., pmxgpar = pmxgpar)
      },
    
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

pmx_initialize <- function(self, private, data_path, input, dv, 
                           config,dvid,cats,conts,occ,strats,settings) {
  if (missing(data_path) || missing(data_path))
    stop("Expecting source path(directory ) and a config path", 
         call. = FALSE)
  if(missing(occ) || is.na(occ)) occ <- ""
  if(missing(cats) || is.na(cats)) cats <- ""
  if(missing(conts) || is.na(conts)) conts <- ""
  if(missing(strats) || is.na(strats)) strats <- ""
  if(missing(settings)) settings <- NULL
  
  private$.data_path <- data_path
  private$.input <- input
  self$config <- config
  self$dv <- dv
  self$dvid <- if(occ!="") occ else dvid
  self$cats <- cats
  self$conts <- conts
  self$occ <- occ
  self$strats <- strats
  self$settings <- settings
  
  ##private$.covariates <- covs[!is.na(covs) & covs!=""]
  self$input <- read_input(input, self$dv,self$dvid,self$cats,self$conts,self$strats)
  self$data <- load_source(sys=config$sys,  private$.data_path,
                           self$config$data)
  self$post_load()
  ## create all plots
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
  dname <- x$dname
  if(!is.null(self$data[[dname]])) {
    private$.plots[[pname]] <- plot_pmx(x, dx = self$data[[dname]])
  } else {
    # throw error - to be improved
    private$.plots[[pname]] <- NULL
    message(sprintf("Error - invalid data set: %s",pname))
  }
  invisible(self)
}

pmx_update_plot <- function(self, private, pname, filter,strat.facet,strat.color, ..., pmxgpar){
  # assertthat::assert_that(isnullOrPmxgpar(pmxgpar))
  x <- private$.plots_configs[[pname]]
  old_class <- class(x)
  old_class_gp <- class(x$gp)
  
  ## update graphical parameters 
  if(!is.null(pmxgpar)) x <- l_left_join(x, pmxgpar)
  newopts <- list(...)
  if(length(newopts)>0){
    hl <- newopts[names(newopts) %in% unique(c(names(x), "shrink"))]
    gpl <- newopts[!names(newopts) %in% unique(c(names(x), "shrink"))]
    hl$gp <- gpl 
    x <- l_left_join(x, hl)
  }
  ## filtering  
  x[["filter"]] <- filter
  ## stratification  
  if(!is.null(strat.color)){
    x[["strat.color"]] <- strat.color
    x[["labels"]][["legend"]] <- strat.color
  }
  if(!is.null(strat.facet)){
    x[["strat.facet"]] <- strat.facet
    x[["labels"]][["title"]] <- 
      sprintf("%s by %s",
              x$gp[["labels"]][["title"]],formula_to_text(strat.facet))
  }
  
  class(x$gp) <- old_class_gp
  class(x) <- old_class
  
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
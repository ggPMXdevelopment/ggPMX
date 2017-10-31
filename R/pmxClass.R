
check_argument <- function(value,pmxname){
  call <- match.call()
  if(missing(value) || is.null(value))
    stop(
      sprintf("Please set a %s argument", 
              deparse(call$value), pmxname)
    )
  value
}


#' Create a pmx object
#' 
#' Create a pmx object from a data source
#' @param config Can be either :
#' The complete path for the configuration file, the name of configuration within the built-in
#' list of configurations, or a configuration object.
#' @param sys the system name can be MLX/NM
#' @param directory \code{character} modelling output directory. 
#' @param input \code{character} complete path to the modelling input file
#' @param dv \code{character} the name of measurable variable used in the input modelling file
#' @param dvid \code{character} observation type parameter
#' @param cats \emph{[Optional]}\code{character} vector of categorical covariates
#' @param conts \emph{[Optional]}\code{character} vector of continuous covariates
#' @param occ \emph{[Optional]}\code{character} occasinal covariate variable name
#' @param strats \emph{[Optional]}\code{character} extra stratification variables
#' @param settings \emph{[Optional]}\code{list} list of global settings parameters 
#' shared between all plots
#' @return a pmxClass object
#' @seealso  \code{\link{pmx_mlx}} 
#' @export
#' @details 
#' 
#' \strong{setting} is a list of global settings shared between all plots. it contains:
#' \itemize{
#' \item {\strong{is.draft:}} {\code{logical} if set to FALSE any plot is without draft annotation}
#' }
#' @examples
## \dontrun{
#'
#'## Example to create the controller using theophylline data
#'theophylline <- file.path(system.file(package = "ggPMX"), "testdata", 
#'                          "theophylline")
#'WORK_DIR <- file.path(theophylline, "Monolix")
#'input_file <- file.path(theophylline, "data_pk.csv")

#' ## using only mondatory varaibles
#' ctr <- pmx(
#'   sys="mlx",
#'   config = "standing",
#'   directory = WORK_DIR,
#'   input = input_file,
#'   dv = "Y",
#'   dvid ="DVID"
#'   )
#' ## Using covariates 
#' ctr <- pmx(
#'   sys="mlx",
#'   config = "standing",
#'   directory = WORK_DIR,
#'   input = input_file,
#'   dv = "Y",
#'   dvid ="DVID",
#'   cats=c("SEX"),
#'   conts=c("WT0","AGE0"),
#'   strats="STUD"
#' )
#' ## using settings paremeter
#' ctr <- pmx(
#'   sys="mlx",
#'   config = "standing",
#'   directory = WORK_DIR,
#'   input = input_file,
#'   dv = "Y",
#'   dvid ="DVID",
#'   settings=list(is.draft=FALSE)
#')
#' 
## }

pmx <-
  function(config, sys=c("mlx","nm"), directory, input, dv,dvid,cats=NULL,conts=NULL,occ=NULL,strats=NULL,
           settings=NULL){
    directory <- check_argument(directory, "work_dir")
    input <- check_argument(input, "input")
    dv <- check_argument(dv, "dv")
    dvid <- check_argument(dvid, "dvid")
    if(missing(cats)) cats <- ""
    assert_that(is_character_or_null(cats))
    if(missing(conts)) conts <- ""
    assert_that(is_character_or_null(conts))
    if(missing(occ)) occ <- ""
    assert_that(is_character_or_null(occ))
    if(missing(strats)) strats <- ""
    assert_that(is_character_or_null(strats))
    if(!inherits(config, "pmxConfig"))
      config <- load_config(config, sys)
    pmxClass$new(directory, input, dv, config,dvid,cats,conts,occ,strats,settings)
  }

#' Wrapper to pmx constructor
#' @param config Can be either :
#' The complete path for the configuration file, the name of configuration within the built-in
#' list of configurations, or a configuration object.
#' @param directory where the files are located. 
#' @param input \code{character} complete path to the modelling input file
#' @param dv \code{character} the name of measurable variable used in the input modelling file
#' @param dvid \code{character} observation type parameter
#' @param cats \emph{[Optional]}\code{character} vector of categorical covariates
#' @param conts \emph{[Optional]}\code{character} vector of continuous covariates
#' @param occ \emph{[Optional]}\code{character} occasinal covariate variable name
#' @param strats \emph{[Optional]}\code{character} extra stratification variables
#' @param settings \emph{[Optional]}\code{list} list of global settings parameters that be shared between all plots
#' @seealso  \code{\link{pmx}} 
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
##' @param strat.facet \code{formula} define categorical stratification as formula
##' @param strat.color \code{character}
##' @param trans \code{list}{transformation operator}
#' @param ... other plot parameters to configure \code{\link{pmx_gpar}}.
#'
#' @family pmxclass
#' @return invisible ctr object
#' @export
set_plot <- function(ctr, ptype = c("IND", "DIS", "RES","ETA_PAIRS","ETA_COV","PMX_QQ"), 
                     pname, 
                     filter =NULL, strat.color=NULL,strat.facet=NULL,trans=NULL,...){
  assert_that(is_pmxclass(ctr))
  ptype <- match.arg(ptype)
  assert_that(is_string_or_null(pname))
  assert_that(is_string_or_null(strat.color)) 
  assert_that(is_string_or_formula_or_null(strat.facet)) 
  
  
  
  conf <-
    switch(ptype,
           IND=individual(...),
           DIS=if(ctr$has_re)distrib(...),
           RES=residual(...),
           ETA_PAIRS=if(ctr$has_re)eta_pairs(...),
           ETA_COV=if(ctr$has_re)eta_cov(...),
           PMX_QQ=pmx_qq(...)
           
    )
  if(!is.null(substitute(filter))){
    filter <- deparse(substitute(filter))
    filter <- local_filter(filter)
  }
  if(!is.null(conf)){
    conf[["filter"]] <- filter
    conf[["trans"]] <- trans
    if(!is.null(strat.color)) conf[["strat.color"]] <- strat.color
    if(!is.null(strat.facet)) conf[["strat.facet"]] <- strat.facet
    ctr[["config"]][["plots"]][[toupper(pname)]] <- 
      c(ptype = ptype, list(...))
    ctr$add_plot(conf, pname)
  }
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
#' p1 <- ctr %>% get_plot("ipred_iwres")
#' ## get all pages or some pages
#' p2 <- ctr %>% get_plot("indiv")
#' ## returns one page of individual plot
#' p2 <- ctr %>% get_plot("indiv",napge=1)
#' p3 <- ctr %>% get_plot("indiv",napge=c(1,3))
#' ## get distribution plot
#' pdistri <- ctr %>% get_plot("ebe_hist")
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

#' Get plots description
#'
#' @param ctr  \code{pmxClass} controller object
#' 
#' @family pmxclass
#' @return data.frame of plots
#' @export

plots <- function(ctr){
  assert_that(is_pmxclass(ctr))
  x <- ctr$config
  if (exists("plots", x)) {
    data.table(
      plot_name=tolower(names(x$plots)),
      plot_type=sapply(x$plots,"[[","ptype"))
  }
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
                                       "eta", "finegrid", "input")){
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
    has_re=FALSE,re=NULL,
    initialize = function(data_path, input, dv, config, dvid, cats, conts, occ,strats,settings)
      pmx_initialize(self, private, data_path, input, dv, config, dvid, cats, conts, occ,strats,settings),
    
    print = function(data_path, config, ...)
      pmx_print(self, private, ...),
    
    # Operations ---------------------------------------------------------------
    add_plot = function(x, pname)
      pmx_add_plot(self, private, x, pname),
    
    update_plot = function(pname, strat.facet=NULL,strat.color=NULL,
                           filter=NULL,trans=NULL,
                           ..., pmxgpar = NULL){
      pmx_update_plot(self, private, pname,
                      strat.color=strat.color, strat.facet = strat.facet,
                      filter,trans,..., pmxgpar = pmxgpar)
    },
    
    remove_plot = function(pname, ...)
      pmx_remove_plot(self, private, pname, ...),
    
    get_config = function(pname)
      pmx_get_config(self, private, pname),
    
    set_config = function(pname,new)
      pmx_set_config(self, private, pname,new),
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
  if(missing(occ) || is.null(occ) || is.na(occ)) occ <- ""
  if(missing(cats) || is.null(cats) || is.na(cats)) cats <- ""
  if(missing(conts) || is.null(conts) || is.na(conts)) conts <- ""
  if(missing(strats) || is.null(strats) || is.na(strats)) strats <- ""
  if(missing(settings)) settings <- NULL
  
  private$.data_path <- data_path
  private$.input <- input
  self$config <- config
  self$dv <- dv
  self$dvid <- dvid
  self$cats <- toupper(cats)
  self$conts <- toupper(conts)
  self$occ <- toupper(occ)
  self$strats <- toupper(strats)
  self$settings <- settings
  
  ##private$.covariates <- covs[!is.na(covs) & covs!=""]
  self$input <- read_input(input, self$dv,self$dvid,self$cats,self$conts,self$strats,self$occ)
  self$data <- load_source(sys=config$sys,  private$.data_path,
                           self$config$data,dvid=self$dvid)
  ## check random effect
  
  if(!is.null(self$data[["eta"]])){
    re <- grep("^eta_(.*)_(mode|mean)",names(self$data[["eta"]]),value=TRUE)
    if(length(re)>0){
      self$has_re <- TRUE
      self$re <- gsub("^eta_(.*)_(mode|mean)","\\1",re)
      self$data[["eta"]] <- 
        post_load_eta(self$data[["eta"]],
                      self$input,self$sys,self$occ)
    }
  }
  
  self$post_load()
  ## create all plots
  for ( nn in names(self$config$plots)){
    x <- self$config$plots[[nn]]
    x$pname <- tolower(nn)
    do.call(set_plot, c(ctr = self, x))
  }
  
}
#' @importFrom knitr kable
pmx_print <- function(self, private, ...){
  cat("\npmx object:\n")
  ctr_table <- 
    rbind(
      c("working directory",
        gsub(path.package("ggPMX"),"",private$.data_path)),
      c("Modelling input",basename(private$.input)),
      c("dv",self$dv),
      c("dvid",self$dvid),
      c("cats",paste(self %>% get_cats,collapse = ",")),
      c("conts",paste(self %>% get_conts,collapse = ",")),
      c("strats",paste(self %>% get_strats,collapse = ","))
    )
  colnames(ctr_table) <- c("PARAM","VALUE")
  
  print(kable(ctr_table))
  
  
  print(self$config, ...)
  
}


pmx_transform <- function(x,dx,trans,direction){
  if(is.character(trans)){
    params <- strsplit(trans,"_")[[1]]
    trans <- params[1]
    direction <- params[2]
  }
  cols_res <- function(x){
    with(x,{
      switch(
        direction,
        x=aess$x,
        y=aess$y,
        xy=c(aess$x,aess$y)
      )
    })
  }
  
  cols_ind <- function(x){
    switch(
      direction,
      x="TIME",
      y=c("PRED","IPRED","DV"),
      xy=c("TIME","PRED","IPRED","DV")
    )
  }
  
  cols_dis <- function(x){
    switch(
      direction,
      x=c("VALUE"),
      y=c("VALUE"),
      xy=c("VALUE")
    )
  }
  
  cols_qq <- function(x){
    switch(
      direction,
      x=x$x
    )
  }
  
  cols_eta_conts <- function(x){
    switch(
      direction,
      y="VALUE"
    )
  }
  
  cols <- switch(
    x[["ptype"]],
    RES=cols_res(x),
    IND=cols_ind(x),
    DIS=cols_dis(x),
    PMX_QQ=cols_qq(x),
    ETA_COV=cols_eta_conts(x)
    
  )
  cols <- intersect(cols ,names(dx))
  if(length(cols)>0)
    dx[,(cols):=lapply(.SD,get(trans)),.SDcols =cols]
  dx
} 



## TODO change the way how we choose the data
## USE AN EXPLICIT METHOD
## data_set(s) for res,data_set(s) for IND,..
pmx_add_plot <- function(self, private, x, pname){
  assert_that(is_pmx_gpar(x))
  if(missing(pname))
    pname <- paste(x$aess, collapse="_")
  
  ##assert_that(is_string_or_expression_or_null(filter))
  
  
  pname <- tolower(pname)
  private$.plots_configs[[pname]] <- x
  ptype <- self[["config"]][["plots"]][[toupper(pname)]][["ptype"]]
  dname <- x$dname
  if(!is.null(self$data[[dname]])) {
    dx <- self$data[[dname]]
    assert_that(is.data.table(dx))
    x$input <- self %>% get_data("input")
    if(!is.null(x[["filter"]])) {
      dx <- x[["filter"]](dx)
      if(ptype=="IND") x$input <- x[["filter"]](x$input)
      
    }
    ## stratification 
    
    
    if(!is.null(x[["strat.color"]])){
      gp <- x[["gp"]]
      gp[["labels"]][["legend"]] <- x[["strat.color"]]
      x[["gp"]] <- gp
    }
    if(!is.null(x[["strat.facet"]])){
      x[["labels"]][["title"]] <- 
        sprintf("%s by %s",
                x$gp[["labels"]][["title"]],formula_to_text(x[["strat.facet"]]))
    }
    if(!is.null( x[["trans"]])) {
      dx1 <- copy(dx)
      dx <- pmx_transform(x,dx1, x[["trans"]])
      if(ptype=="IND") {
        inp <- copy(x$input)
        x$input <- pmx_transform(x,inp, x[["trans"]])
      }
    }
    if(ptype=="DIS"){
      VAR <- FUN <- NULL
      dx <- dx[VAR == "eta" & grepl("mode", FUN)]
    }
    if(ptype=="ETA_COV"){
      x[["cats"]] <- self %>% get_cats
      x[["conts"]] <- self %>% get_conts
    }
    if(!is.null(x[["has.shrink"]]) && x$has.shrink){
      grp <- as.character(unlist(lapply(x[["strat.facet"]],as.list)))
      grp <- intersect(grp,names(dx))
      x[["shrink.dx"]] <- 
        if(!is.null(x[["filter"]])){
          ff <- x[["filter"]]
          shrinkage(
            ff(self$data[["estimates"]]),ff(self$data[["eta"]]),
            fun = x$shrink$fun,by=grp)
        }else
          shrinkage(
            self$data[["estimates"]],self$data[["eta"]],
            fun = x$shrink$fun,by=grp)
    }
    
    if(!is.null(self$settings)){
      if("is.draft" %in% names(self$settings))
        x$gp$is.draft <- self$settings$is.draft
      
    }
    self$set_config(pname,x)
    
    private$.plots[[pname]] <- plot_pmx(x, dx = dx)
    
  } else {
    # throw error message
    private$.plots[[pname]] <- NULL
    message(sprintf("No data %s provided for plot %s",
                    sprintf('%s',dname),sprintf('%s',pname)))
  }
  invisible(self)
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

pmx_set_config <- function(self,private, pname,new){
  pname <- tolower(pname)
  private$.plots_configs[[pname]] <- new
}


pmx_get_plot <- function(self, private, pname){
  pname <- tolower(pname)
  private$.plots[[pname]]
}

pmx_plots <- function(self, private){
  names(private$.plots)
}

pmx_post_load <- function(self, private){
  self$data <- post_load(
    self$data, self$input, self$config$sys, 
    self$config$plots,
    occ       =get_occ(self))
  
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



#' Creates a deep copy of the controller
#'
#' @param ctr \code{pmxClass} object
#'
#' @return an object of \code{pmxClass}
#' @export
#' @details 
#' 
#' The controller is an `R6` object, it behaves like a reference object.  
#' Some functions ( methods) can have a side effect on the controller and modify it internally. 
#' Technically speaking we talk about chaining not piping here. However , 
#' using \code{pmx_copy} user can work on a copy of the controller.

#'
#' @examples
#'  ctr <- theophylline()
#'  cctr <- ctr %>% pmx_copy
#'  ## Any chnage in the ctr has no side effect in the ctr and vice versa 
pmx_copy <- function(ctr){
  assert_that(is_pmxclass(ctr))
  ctr$clone()
}
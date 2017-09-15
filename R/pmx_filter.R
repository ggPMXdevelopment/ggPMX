#' filter data in a pmx controller
#'
#' @param ctr A controller. An object of 'pmxClass'
#' @param data_set A data_set within the controller to apply a filter to.
#' @param pmx_exp A filter expression
#'
#' @return Returns a pmx controller with a filtered data set.
#' @export
#' @examples 
#' \dontrun{
#' ## example of global filter
#' ctr <- pmx_mlx("standing")
#' ctr %>% pmx_filter(data_set = "prediction", ID == 5 & TIME <2)
#' ctr %>% get_data("prediction")
#' }
pmx_filter <- 
  function(ctr, data_set = c("estimates","predictions", 
                            "eta", "finegrid", "shrink"), pmx_exp){
    assert_that(is_pmxclass(ctr))
    data_set <- match.arg(data_set)
    assert_that(is_language_or_string(substitute(pmx_exp)))
    if(is_string(substitute(pmx_exp))){
      pmx_exp <- expression(pmx_exp)
    }
    oldData <- ctr[["data"]][[data_set]]
    e <- substitute(pmx_exp)
    r <- eval(e, oldData, parent.frame())
    if (!is.logical(r)) 
      stop("'expression' must evaluate to logical")
    r <- r & !is.na(r)
    
    newData <- oldData[r]
    ctr[["data"]][[data_set]] <- newData
    
    ## update all plots after global filtering
    for ( nn in ctr %>% plot_names())
       ctr %>% pmx_update(pname = nn)
    ctr
  }

#' Update plot object
#'
#' @param ctr  \code{pmxClass} controller object
#' @param pname character the plot name to update
#' @param filter optional filter which will be applied to plotting data
#' @param strat.facet \code{formula} optional stratification parameter
#' @param strat.color \code{character} optional stratification parameter
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


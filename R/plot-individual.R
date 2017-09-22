
#' Create individual fits object
#'
#' @param labels  plot tesxts. labels, axis,
#' @param facets list facets settings nrow/ncol
#' @param dname name of dataset to be used
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#'
#' @return individual fit object
#' @export
#' @family plot_pmx
#' @seealso \code{\link{plot_pmx.individual}}
#'
#'@examples
#'## get individual plot using package default data set
#'library(ggPMX)
#'ctr <- theophylline()
#'## display the first page of the individual plot
#'ctr %>% get_plot("indiv",1)
# display all pages ( default)
#'ctr %>% get_plot("indiv")


individual <- function(labels, 
                       facets = list(ncol = 3, nrow = 4), 
                       dname = NULL,
                       ...){
  assert_that(is_list(facets))
  assert_that(is_string_or_null(dname))
  if(missing(labels))
    labels <- list(
      title = "Individual fits",
      subtitle = "",
      x = "Time after first dose (hours)",
      y = "ABC123 plasma concentration (ng/mL)")
  assert_that(is_list(labels))
  if(is.null(dname)) dname <- "IND"
  
  structure(list(
    ptype="IND",
    dname=dname,
    aess = list(x = "TIME", y1 = "PRED", y2 = "IPRED"),
    labels = labels,
    point = list(shape = 2, color = "grey50", size = 1),
    facets = facets,
    gp = pmx_gpar(labels = labels,  ...)
    
    
  ), class = c("individual", "pmx_gpar"))
}



#' Plot individual fits

#' @param x individual object
#' @param dx data set
#' @param ... not used for the moment
#'
#' @return a list of ggplot2
#' @export
#' @import ggplot2
#' @import data.table
#' @family plot_pmx
#'
plot_pmx.individual <-
  function(x, dx,...){
    ID <- NULL
    ## plot
    ## dx <- dx[DVID==1]
    strat.facet <- x[["strat.facet"]]
    strat.color <- x[["strat.color"]]
    
    wrap.formula <- if(!is.null(strat.facet)) wrap_formula(strat.facet,"ID")
    else formula("~ID")
    
    get_page <- with(x,{
      p <- ggplot(dx, aes(TIME, DV))+
        geom_line(aes(y=IPRED),size=1,linetype=1)+
        geom_line(aes(y=PRED),size=1,linetype=2)+
        with(point,geom_point(shape=shape,size=size,color=color))
      p <- plot_pmx(gp, p)
      
      ## split pages
      npages <- ceiling(with(facets, 
                             length(unique(dx$ID)) / nrow / ncol))
      
      with(facets, function(i){
        res <- list()
        if (is.null(i))i <- seq_len(npages)
        i <- intersect(i,seq_len(npages))
        res <- lapply(i,function(x){
          cat("page:",x,"\n")
          p + facet_wrap_paginate(wrap.formula, ncol = ncol, nrow = nrow, 
                                  page = x)
        }
        )
        if(length(res)==1)res[[1]] else res 
      }
      )
    })
    
    get_page
    
    
  }


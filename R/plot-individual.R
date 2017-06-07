
#' Create individual fits object
#'
#' @param labels  plot tesxts. labels, axis,
#' @param has.curve  logical if TRUE add predictions lines
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
#'ctr <- pmx_mlx(config = "standing")
#'## display the first page of the individual plot
#'ctr %>% get_plot("indiv",1)
individual <- function(labels, has.curve = TRUE, 
                       facets = list(ncol = 3, nrow = 4), 
                       dname = NULL,
                       ...){
  assert_that(is_logical(has.curve))
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
    aess = list(x = "TIME", y1 = "PRED", y2 = "IPRED"),
    labels = labels,
    point = list(shape = 2, color = "grey50", size = 1),
    has.curves = has.curve,
    facets = facets,
    gp = pmx_gpar(labels = labels, dname=dname, ...)
    
    
  ), class = c("individual", "pmx_gpar"))
}



#' Plot individual fits

#' @param x individual object
#' @param dx data set
#' @param include list of individual ID to plot, if missing all ID will be plotted
#'
#' @return a list of ggplot2
#' @export
#' @import ggplot2
#' @import data.table
#' @family plot_pmx
#'
plot_pmx.individual <-
  function(x, dx, include){
    assert_that(is_pmx_gpar(x))
    if(!is.null(x[["filter"]])){
      dx <- x[["filter"]](dx)
    }
    
    ##reshape data to the long format
    if(!missing(include))
      dx <- setDT(dx)[ID %in% include]
    dat <- melt(dx,
                id.vars=c("ID", "TIME", "DV"),
                measure.vars = c("PRED", "IPRED"))
    ## plot
    get_page <- with(x,{
      p <- ggplot(dat, ggplot2::aes(TIME, DV, linetype = variable)) +
        with(point, geom_point(shape = shape, color = color, 
                               size = size),na.rm=TRUE)
      if(has.curves)
        p <- p + geom_line(ggplot2::aes(y = value), size = 1,na.rm=TRUE)
      
      p <- plot_pmx(gp, p)
      
      ## split pages
      npages <- ceiling(with(facets, 
                             length(unique(dx$ID)) / nrow / ncol))
      with(facets, function(i)
        if(i<=npages){
          p + facet_wrap_paginate(~ID, ncol = ncol, 
                                  nrow = nrow, page = i)
        }
      )
    })
    
    get_page
    
    
  }


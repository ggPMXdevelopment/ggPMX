
#' Create individual fits object
#'
#' @param labels  plot tesxts. labels, axis,
#' @param has.curve  logical if TRUE add predictions lines
#' @param facets list facets settings nrow/ncol
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#'
#' @return individual fit object
#' @export
#' @seealso \code{\link{plot_pmx.individual}}
#'
#'
individual <- function(labels, has.curve = TRUE, 
                       facets = list(ncol = 3, nrow = 4), 
                       ...){
  if(missing(labels))
    labels <- list(
      title = "Individual fits",
      subtitle = "(MLX)",
      x = "Time after first dose (hours)",
      y = "ABC123 plasma concentration (ng/mL)")
  structure(list(
    aess = list(x = "TIME", y1 = "PRED", y2 = "IPRED"),
    labels = labels,
    point = list(shape = 2, color = "grey50", size = 1),
    has.curves = has.curve,
    facets = facets,
    gp = pmx_gpar(labels = labels, ...)


  ), class = c("individual", "pmx_gpar"))
}



#' Plot individual fits

#' @param x individual object
#' @param dx data set
#' @param include list of individual ID to plot, if missing all ID will be plotted
#'
#' @return a list of ggplot2
#' @export
#' @seealso \code{\link{individual}}
#' @family plot_pmx
#'
plot_pmx.individual <-
  function(x, dx, include){
    stopifnot(is.pmx_gpar(x))
    ID <- NULL
    ##reshape data to the long format
    if(!missing(include))
      dx <- data.table::setDT(dx)[ID %in% include]
    dat <- data.table::melt.data.table(
      dx,
      id.vars=c("ID", "TIME", "DV"),
      measure.vars = c("PRED", "IPRED"))
    ## plot
    with(x,{
      p <- ggplot2::ggplot(dat, ggplot2::aes(TIME, DV, 
                                             linetype = variable)) +
        with(point, ggplot2::geom_point(shape = shape, color = color, 
                                        size = size))
           if(has.curves)
             p <- p + ggplot2::geom_line(ggplot2::aes(y = value), size = 1)

           p <- plot_pmx(gp, p)

           ## split pages
           npages <- ceiling(with(facets, 
                                  length(unique(dat$ID) / nrow / ncol)))
           with(facets,
                lapply(seq_len(npages), function(i)
                  p <- p + facet_wrap_paginate(~ID, ncol = ncol, 
                                               nrow = nrow, page = i)
                ))
         })

  }


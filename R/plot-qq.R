



#' creates an eta_cov object to plot ebe versus covariates
#'
#' @param x \code{character} variable name to sample
#' @param labels list of texts/titles used within the plot
#' @param dname name of dataset to be used
#' @param point \code{list}
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return \code{eta_cov} object
#' @family plot_pmx
#' @details 
#' 
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default  "EBE vs. covariates"}
#' \item {\strong{x:}} {x axis label default to "Etas"}
#' \item {\strong{y:}} {y axis label default to empty}
#' }
#'
#'#' \strong{point} is a list that contains:
#' \itemize{
#' \item {\strong{shape:}} {default to 1}
#' \item {\strong{color:}} {default to black}
#' \item {\strong{size:}} {default to 1}
#' }
#' 
#' @export
pmx_qq <- function(
  x,
  labels,
  dname = NULL,
  point=NULL,
  ...){
  assert_that(is_string_or_null(dname))
  if(is.null(dname)) dname <- "predictions"
  
  
  if(missing(labels))
    labels <- list(
      title = sprintf("QQ plot: %s",x)
    )
  labels$y <- ""
  labels$x <- ""
  assert_that(is_list(labels))
  default_point <- list(shape = 1, color = "black", size = 1)
  point <- l_left_join(default_point, point)
  labels$subtitle <- ""
  structure(list(
    ptype="PMX_QQ",
    x=x,
    dname = dname,
    point=point,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      has.smooth = FALSE)
    
  ), class =c("pmx_qq", "pmx_gpar"))
}




















#' Plot EBE versus covariates
#'
#' @param x eta_cov object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggplot2 plot
#' @export
#' @seealso \code{\link{eta_cov}}
#' @family plot_pmx
#' @import ggplot2
#'
plot_pmx.pmx_qq <- function(x, dx,...){
  p <- ggplot(dx, aes_string(sample=x$x))+
    geom_abline()+
    with(x$point,geom_point(stat='qq',shape = shape, color = color, size = size))+
    coord_cartesian(xlim=c(-4,4),ylim=c(-4,4))
  if(!is.null( x[["strat.facet"]])){
    if(is.character(strat.facet))
      strat.facet <- formula(paste0("~", x[["strat.facet"]]))
    p <- p + facet_grid(strat.facet)
  }
  
  if(!is.null(p)) p <- plot_pmx(x$gp, p)
  p
}



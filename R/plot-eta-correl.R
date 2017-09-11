

#' Creates eta correlation object
#'
#' @param title character the plot title
#' @param dname name of dataset to be used
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#' @return ecorrel object
#' @family plot_pmx
#' @export
ecorrel <- function(title,dname=NULL,...){
  assert_that(is_string_or_null(dname))
  if(is.null(dname)) dname <- "eta"
  if(missing(title)) title <- "Correlation random effect"
  labels <- list(
    title = title,
    subtitle ="",
    x = "",
    y = "")
  structure(list(
    dname = dname,
    labels=labels,
    point = list(shape = 2, color = "grey50", size = 1),
    gp = pmx_gpar(
      labels=labels,
      discrete = FALSE,
      has.smooth = FALSE,
      has.band = FALSE, ...)
    
  ), class =c("ecorrel", "pmx_gpar"))
}

lower.plot <- function(data, mapping, method = "loess", gp,point) {
  p <- 
    ggplot(data = data, mapping = mapping) +
    with(point,geom_point(shape=shape,size=size,color=color))+
    geom_smooth(method = method, se=FALSE, size=1,color='black')
  plot_pmx(gp, p)

}

diag.plot <- function(data, mapping, gp) {
  p <-  ggally_densityDiag(data = data, mapping = mapping) 
  plot_pmx(gp, p)
}


upper.plot <- function(data, mapping, gp) {
  p <-  ggally_cor(data = data, mapping = mapping) 
  plot_pmx(gp, p)
}







#' Plot random effect correlation plot
#'
#' @param x distribution object
#' @param dx data set
#' @return ggpairs plot
#' @export
#' @seealso \code{\link{distrib}}
#' @family plot_pmx
#' @import ggplot2
#' @import GGally
#'
plot_pmx.ecorrel <- function(x, dx){
  ## avoid RCMDCHECK warning
  ID <- VARIABLE <- VALUE <- NULL
  assert_that(is_pmx_gpar(x))
  assert_that(is.data.table(dx))
  
  if(!is.null(x[["filter"]])){
    dx <- x[["filter"]](dx)
  }
  
  data_plot <- dcast(dx[,list(ID,VARIABLE,VALUE)],ID~VARIABLE,fun.aggregate = max,value.var = "VALUE")[,-"ID",with=F]
  
  p <- with(x, {
    ggpairs(
      data_plot, 
      lower = list(continuous = wrap(lower.plot, method = "loess",gp=gp,point=point)),
      diag = list(continuous = wrap(diag.plot,gp=gp)),
      upper = list(continuous = wrap(upper.plot,gp=gp)),
      title = labels$title,
      xlab=labels$x,
      ylab=labels$y)
    
  })
  p
  
}

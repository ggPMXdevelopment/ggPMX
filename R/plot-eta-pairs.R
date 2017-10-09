

#' Creates eta correlation object
#'
#' @param title character the plot title
#' @param dname name of dataset to be used
#' @param type.eta \code{character} type of eat can be 'mode' or 'mean'.'mode' byd efault
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#' @return ecorrel object
#' @family plot_pmx
#' @export
eta_pairs <- function(title,dname=NULL,type.eta=c("mode","mean"),
                      text_color="black",...){
  assert_that(is_string_or_null(dname))
  if(is.null(dname)) dname <- "eta"
  if(missing(title)) title <- "Correlations of random effects"
  labels <- list(
    title = title,
    subtitle ="",
    x = "",
    y = "")
  structure(list(
    dname = dname,
    labels=labels,
    point = list(shape = 1, color = "grey50", size = 1),
    type.eta =  match.arg(type.eta),
    text_color=text_color,
    gp = pmx_gpar(
      ptype="ECORREL",
      labels=labels,
      discrete = FALSE,
      has.smooth = FALSE,
      has.band = FALSE, ...)
    
  ), class =c("eta_pairs", "pmx_gpar"))
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


upper.plot <- function(data, mapping, gp,text_color) {
  p <-  ggally_cor(data = data, mapping = mapping,colour=text_color) 
  plot_pmx(gp, p)
}







#' Plot random effect correlation plot
#'
#' @param x distribution object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggpairs plot
#' @export
#' @seealso \code{\link{distrib}}
#' @family plot_pmx
#' @import ggplot2
#' @import GGally
#'
plot_pmx.eta_pairs <- function(x, dx,...){
  ## avoid RCMDCHECK warning
  ID <- VARIABLE <- VALUE <- FUN <- NULL
  
 
  ## filter by type of eta 
  dx <- dx[FUN==x$type.eta]
  if(nrow(dx)==0)
    stop("Now rows find for eta of type ",x$type.eta,"\n")
  
  data_plot <- dcast(dx[,list(ID,EFFECT,VALUE)],ID~EFFECT,fun.aggregate = max,value.var = "VALUE")[,-"ID",with=F]
  
  p <- with(x, {
    ggpairs(
      data_plot, 
      lower = list(continuous = wrap(lower.plot, method = "loess",gp=gp,point=point)),
      diag = list(continuous = wrap(diag.plot,gp=gp)),
      upper = list(continuous = wrap(upper.plot,gp=gp,text_color=text_color)),
      title = labels$title,
      xlab=labels$x,
      ylab=labels$y)
    
  })
  p
  
}

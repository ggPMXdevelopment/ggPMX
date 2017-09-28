
library(ggplot2)





#' creates an eta_cov object to plot ebe versus covariates
#'
#' @param labels list of texts/titles used within the plot
#' @param type box for cats or conts
#' @param dname name of dataset to be used
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
#' @export
eta_cov <- function(
  labels,
  type = c("cats", "conts"),
  dname = NULL,
  ...){
  type <- match.arg(type)
  assert_that(is_string_or_null(dname))
  if(is.null(dname)) dname <- "eta"
  
  
  if(missing(labels))
    labels <- list(
      title = "EBE vs. covariates",
      x = "",
      y = "")
  assert_that(is_list(labels))
  labels$subtitle <- ""
  structure(list(
    ptype="ETA_COV",
    dname = dname,
    type=type,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      has.smooth = FALSE)
    
  ), class =c("eta_cov", "pmx_gpar"))
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
plot_pmx.eta_cov <- function(x, dx,...){
  p <- if(x$type=="cats"){
    cats  <- x[["cats"]]
    dx.cats <- dx[,c(cats,"VALUE","EFFECT"),with=FALSE]
    ggplot(melt(dx.cats,measure.vars = cats)) + 
      geom_boxplot(aes(x=value,y=VALUE)) +
      facet_grid(EFFECT~variable,scales = "free")
  }else{
    conts <- x[["conts"]]
    dx.conts <- dx[,c(conts,"VALUE"),with=FALSE]
    dx.conts <- melt(dx.conts,id="VALUE")
    dx.conts[,value:=log10(value)-mean(log10(value)),variable]
    
    ggplot(dx.conts,aes(x=value,y=VALUE)) + 
      geom_point() +
      facet_grid(~variable,scales="free_x") +
      geom_smooth(method = "lm",se=FALSE)
  }
  
  plot_pmx(x$gp, p)
}



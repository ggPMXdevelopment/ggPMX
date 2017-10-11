
#' creates a graphic distribution object
#'
#' @param labels list of texts/titles used within the plot
#' @param jitter list set jitter parameter
#' @param facets list set the facet setting in case of histogram plot
#' @param type box for boxplot or histogram
#' @param dname name of dataset to be used
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#' @param has.shrink \code{logical} if TRUE add shrinkage layer
#' @param shrink \code{list} list of parameters to tune the shrinkage
#' @param has.jitter \code{logical} if TRUE add jitter operator for points

#'
#' @return distrib object
#' @family plot_pmx
#' @details 
#' 
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default "EBE distribution"}
#' \item {\strong{subtitle:}} {plot subtitle default empty}
#' \item {\strong{x:}} {x axis label default to "Etas"}
#' \item {\strong{y:}} {y axis label default to empty}
#' \item {\strong{legend:}} {legend titile default to "random Effect"}
#' }
#' \strong{shrink} is a list that contains:
#' \itemize{
#' \item {\strong{fun:}} {shrinkage function can be \code{sd} or \code{var}}
#' \item {\strong{size:}} {shrinkage text size}
#' \item {\strong{color:}} {shrinkage text color}
#' \item {\strong{vjust:}} {shrinkage position vertical adjustment}
#' }
#'
#'
#' @export
distrib <- function(
  labels,
  has.jitter = TRUE,
  jitter = list(shape = 1, color = "grey50", width = 0.1),
  facets = list(scales = "free_y", nrow = 3),
  type = c("box", "hist"),
  has.shrink = FALSE,
  binwidth=1/30,
  shrink=list(
    fun="sd",size=5,color="black",
    x_=-Inf,y_=Inf, hjust=-1,vjust=5),
  dname = NULL,
  ...){
  assert_that(is_logical(has.jitter))
  assert_that(is_list(jitter))
  assert_that(is_list(facets))
  type <- match.arg(type)
  assert_that(is_logical(has.shrink))
  assert_that(is_list(shrink))
  assert_that(is_string_or_null(dname))
  if(is.null(dname)) dname <- "eta"
  
  
  if(missing(labels))
    labels <- list(
      title = "EBE distribution",
      subtitle = "",
      x = "Etas",
      y = "",
      legend = "Random effect")
  assert_that(is_list(labels))
  
  structure(list(
    ptype="DIS",
    dname = dname,
    aess = list(x = "EFFECT", y = "VAR", z = "FUN"),
    type = type,
    has.jitter = has.jitter,
    jitter = jitter,
    facets = facets,
    binwidth=binwidth,
    has.shrink = has.shrink,
    shrink=shrink,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      has.smooth = FALSE,
      has.band = FALSE, ...)
    
  ), class =c("distrib", "pmx_gpar"))
}



is.formula <- function(x) inherits(x, "formula")

#' merge facets formula with new formula
#'
#' @param x \code{formula} object
#' @param origin the origin formula defualt to ~lfacets 
#'
#' @return \code{formula} object
#' @importFrom stats formula

wrap_formula <- function(x,origin="lfacet"){
  str <- sprintf("~ %s",origin)
  if(is.character(x) && length(x)==1)
    str <- sprintf( "%s ~ %s", x,origin)
  
  if(length(x)==3 && is.formula(x))
    str <- sprintf( "%s ~ %s + %s", deparse(x[[2]]),
                    deparse(x[[3]]),origin)
  
  if(length(x)==2 && is.formula(x))
    str <- sprintf( "%s ~ %s", deparse(x[[2]]),origin)
  return(formula(str))
}

jitter_layer <- function(jitter){
  with(jitter,
       geom_jitter(
         aes_string(x="EFFECT",y = "VALUE"),
         shape = shape, color = color,
         position = 
           position_jitter(width = width,height = 0.1)
       ))
}

distrib.hist <- function(dx,strat.facet,strat.color,x){
  wrap.formula <- if(!is.null(strat.facet)) wrap_formula(strat.facet,"EFFECT")
  else formula("~EFFECT")
  
  p <-   ggplot(data = dx,aes_string(x = "VALUE")) + 
    with(x,geom_histogram(binwidth=binwidth))
  if(!is.null(strat.color))
    p <- p  %+% with(x,geom_histogram(
      aes_string(fill=strat.color),
      position = "dodge",
      binwidth=binwidth))
  p <- p + with(x$facets, facet_wrap(wrap.formula , scales = scales,
                                     nrow = nrow))
  
  if(x$has.shrink) p <- p + shrinkage_layer(x[["shrink.dx"]] ,x$shrink)
  
  p
  
}

distrib.box <- function(dx,strat.color,strat.facet,x){
  
  EFFECT <- VALUE <- NULL
  p <- ggplot(data = dx) +
    geom_boxplot(aes(x=EFFECT, y = VALUE), outlier.shape = NA) 
  
  if(!is.null(strat.color))
    p <-  ggplot(data = dx,aes_string(fill=strat.color)) +
    geom_boxplot(aes_string(x="EFFECT",y = "VALUE"),
                 outlier.shape = NA,position = position_dodge(width = 0.9))
  
  if(!is.null(strat.facet))
    p <-  p + with(x$facets, facet_wrap(strat.facet , scales = scales,
                                        nrow = nrow))
  
  if(x$has.jitter) p <- p + jitter_layer(x$jitter )
  if(x$has.shrink) p <- p + shrinkage_layer(x[["shrink.dx"]] ,x$shrink,"box")
  
  p
  
  
}




shrinkage_layer <- 
  function(dx,shrink,type="hist") {
    ## 
    SHRINK <- EFFECT <- POS <- NULL
    res <-   geom_text(data=dx,
                       aes(x=EFFECT,y=POS,
                           label = sprintf('shrinkage=%s%%',round(SHRINK*100))),
                       color = shrink$color, size = shrink$size,
                       position = position_dodge(width = 0.9)) 
    if(type=="hist")
      res <- 
      with(shrink,annotate(geom="text",
               label = sprintf('shrinkage=%s%%',round(dx$SHRINK*100)),
               x=x_,y=y_,hjust=hjust,vjust=vjust,
               color=color,inherit.aes=FALSE))
               
    res
    
  }











plot_distribution <- function(x,dx,...){
  
  strat.facet <- x[["strat.facet"]]
  strat.color <- x[["strat.color"]]
  p <- if(x$type=="box")distrib.box(dx,strat.color,strat.facet,x)
  else distrib.hist(dx,strat.facet,strat.color,x)
  plot_pmx(x$gp, p)
}





#' Plot EBE distribution
#'
#' @param x distribution object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggplot2 plot
#' @export
#' @seealso \code{\link{distrib}}
#' @family plot_pmx
#' @import ggplot2
#'
plot_pmx.distrib <- function(x, dx,...){
  plot_distribution(x,dx,...)
}

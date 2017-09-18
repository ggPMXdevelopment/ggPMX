
#' creates a graphic distribution object
#'
#' @param labels list of texts/titles used within the plot
#' @param jitter list set jitter parameter
#' @param facets list set the facet setting in case of histogram plot
#' @param type box for boxplot or histogram
#' @param dname name of dataset to be used
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#' @param has.shrink \code{logical} if TRUE add shrinkage layer
#' @param has.jitter \code{logical} if TRUE add jitter operator for points

#'
#' @return distrib object
#' @family plot_pmx
#' @export
distrib <- function(
  labels,
  has.jitter = TRUE,
  jitter = list(shape = 1, color = "grey50", width = 0.1),
  facets = list(scales = "free_y", nrow = 3),
  type = c("box", "hist"),
  has.shrink = FALSE,
  shrink=list(fun="sd",size=5,color="green",vjust=0,dodge=0.9,x=0,y=0),
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
#'
#' @return \code{formula} object
#' @importFrom stats formula

wrap_formula <- function(x,default="lfacet"){
  str <- sprintf("~ %s",default)
  if(is.character(x) && length(x)==1)
    str <- sprintf( "%s ~ %s", x,default)
  
  if(length(x)==3 && is.formula(x))
    str <- sprintf( "%s ~ %s + %s", deparse(x[[2]]),
                    deparse(x[[3]]),default)
  
  if(length(x)==2 && is.formula(x))
    str <- sprintf( "%s ~ %s", deparse(x[[2]]),default)
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
  
  p <-   ggplot(data = dx) +
    if(!is.null(strat.color))
      geom_histogram(aes_string(x = "VALUE",fill=strat.color),
                     position = "dodge") 
  else geom_histogram(aes_string(x = "VALUE")) 
  
  
  p <- p + with(x$facets, facet_wrap(wrap.formula , scales = scales,
                                     nrow = nrow))
  
  if(x$has.shrink) p <- p + shrinkage_layer(x[["shrink.dx"]] ,x$shrink)
  
  p
  
}

distrib.box <- function(dx,strat.color,strat.facet,x){
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
   SHRINK <- NULL
   x_ <- shrink$x
   y_ <- shrink$y
   res <-   geom_text(data=dx,
        aes(x=EFFECT,y=POS,
            label = sprintf('%s%%',round(SHRINK*100))),
        color = shrink$color, size = shrink$size,
        position = position_dodge(width = 0.9),vjust=shrink$vjust) 
   if(type=="hist")
      res <- geom_text(data=dx,
                      x=x_,y=y_,
            aes(label = sprintf('%s%%',round(SHRINK*100))),
        color = shrink$color, size = shrink$size,vjust=shrink$vjust)
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


#' creates a graphic distribution object
#'
#' @param labels list of texts/titles used within the plot
#' @param jitter list set jitter parameter
#' @param facets list set the facet setting in case of histogram plot
#' @param type box for boxplot or histogram
#' @param dname name of dataset to be used
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return distrib object
#' @family plot_pmx
#' @export
distrib <- function(
  labels,
  has.jitter = TRUE,
  jitter = list(shape = 1, color = "grey50", width = 0.1),
  facets = list(scales = "free", nrow = 3),
  type = c("box", "hist"),
  has.shrink = FALSE,
  dname = NULL,
  ...){
  assert_that(is_logical(has.jitter))
  assert_that(is_list(jitter))
  assert_that(is_list(facets))
  type <- match.arg(type)
  assert_that(is_logical(has.shrink))
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
    dname = dname,
    aess = list(x = "EFFECT", y = "VAR", z = "FUN"),
    type = type,
    has.jitter = has.jitter,
    jitter = jitter,
    facets = facets,
    has.shrink = has.shrink,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      has.smooth = FALSE,
      has.band = FALSE, ...)
    
  ), class =c("distrib", "pmx_gpar"))
}




#' Plot EBE distribution
#'
#' @param x distribution object
#' @param dx data set



#'
#' @return ggplot2 plot
#' @export
#' @seealso \code{\link{distrib}}
#' @family plot_pmx
#' @import ggplot2
#'
plot_pmx.distrib <- function(x, dx){
  
  assert_that(is_pmx_gpar(x))
  assert_that(is.data.table(dx))

  if(!is.null(x[["filter"]])){
    dx <- x[["filter"]](dx)
  }
  
  VAR <- NULL; FUN <- NULL
  dx.etas <- dx[VAR == "eta" & grepl("mode", FUN)]
  
  strat.facet <- x[["strat.facet"]]
  strat.color <- x[["strat.color"]]
  
  
  p <- with(x, {
    
    if(has.shrink){
      dx.etas <- merge(dx.etas, shrink, by = "EFFECT")[
        , lfacet := sprintf('%s: with shrinkage %s%%', EFFECT, 
                            round(SHRINK*100))]
      
    }else dx.etas[, lfacet := EFFECT]
    
    wrap_formula <- if(is.null(strat.facet)) as.formula("~lfacet") 
                       else as.formula(paste(strat.facet,"lfacet",sep="~"))
    
    p <- ggplot(dx.etas, aes(EFFECT))
    if(type=="box"){
      p <- if(is.null(strat.color))
        p + geom_boxplot(aes(y = VALUE), outlier.shape = NA)
      else  p + geom_boxplot(aes_string(y = "VALUE",fill=strat.color), outlier.shape = NA)
      if(!is.null(strat.facet))
        p <- p + facet_wrap(strat.facet)
        
        
      if(has.jitter)
        p <- p +
          with(jitter,
               geom_jitter(
                 aes(y = VALUE),
                 shape = shape, color = color,
                 position = 
                   position_jitter(width = width,height = 0.1)
               ))
      if(has.shrink)
        p <- p +
          geom_text(
            data = dx.etas[, list(pos = max(VALUE) * .75,
                                  label = sprintf('%s%%', 
                                                  round(SHRINK[1]*100))
            ),
            EFFECT],
            aes(label = label, y = pos), color = "red", size = 5)
      
      
      
    }else{
      p <- p +  geom_histogram(aes(x = VALUE)) +
        with(facets, facet_wrap(wrap_formula, scales = scales, 
                                         nrow = nrow))
    }
    plot_pmx(gp, p)
  })
  p
  
}

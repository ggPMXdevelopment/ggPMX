
#' creates a graphic distribution object
#'
#' @param labels list of texts/titles used within the plot
#' @param jitter list set jitter parameter
#' @param facets list set the facet setting in case of histogram plot
#' @param type box for boxplot or histogram
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return distrib object
#' @family plot_pmx
#' @export
distrib <- function(
  labels,
  has.jitter = TRUE,
  jitter = list(shape = 2, color = "grey50", width = 1),
  facets = list(scales = "free", nrow = 3),
  type = c("box", "hist"),
  has.shrink = FALSE,
  ...){
  assert_that(is_logical(has.jitter))
  assert_that(is_list(jitter))
  assert_that(is_list(facets))
  type <- match.arg(type)
  assert_that(is_logical(has.shrink))
  
  if(missing(labels))
    labels <- list(
      title = "EBE distribution",
      subtitle = "(MLX)",
      x = "Etas",
      y = "",
      legend = "Random effect")
  assert_that(is_list(labels))
  
  structure(list(
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
#'
plot_pmx.distrib <- function(x, dx){
  
  assert_that(is_pmx_gpar(x))
  if(!is.null(x[["filter"]])){
    dx <- x[["filter"]](dx)
  }
  
  VAR <- NULL; FUN <- NULL
  dx.etas <- dx[VAR == "eta" & grepl("mean", FUN)]
  
  p <- with(x, {
    
    if(has.shrink){
      dx.etas <- merge(dx.etas, shrink, by = "EFFECT")[
        , lfacet := sprintf('%s: with shrinkage %s%%', EFFECT, 
                            round(SHRINK*100))]
      
    }else dx.etas[, lfacet := EFFECT]
    
    p <- ggplot2::ggplot(dx.etas, aes(EFFECT, fill = EFFECT))
    if(type=="box"){
      p <- p + ggplot2::geom_boxplot(aes(y = VALUE), outlier.shape = NA)
      if(has.jitter)
        p <- p +
          with(jitter,
               ggplot2::geom_jitter(
                 ggplot2::aes(y = VALUE),
                 shape = shape, color = color,
                 position = ggplot2::position_jitter(width = width)
               ))
      if(has.shrink)
        p <- p +
          ggplot2::geom_text(
            data = dx.etas[, list(pos = max(VALUE) * .75,
                                  label = sprintf('%s%%', 
                                                  round(SHRINK[1]*100))
            ),
            EFFECT],
            ggplot2::aes(label = label, y = pos), color = "red", size = 5)
      
      
      
    }else{
      p <- p +  ggplot2::geom_histogram(ggplot2::aes(x = VALUE)) +
        with(facets, ggplot2::facet_wrap(~lfacet, scales = scales, 
                                         nrow = nrow))
    }
    plot_pmx(gp, p)
  })
  p
  
}

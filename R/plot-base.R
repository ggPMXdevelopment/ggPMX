

#' The ggPMX base plot function
#'
#'
#' This function should be called internally by other plots to set
#' general settings like , smoothing, add band, labelling, theming,...
#' @param gpar object of pmx_gpar type
#' @param p plot
#' @import ggplot2
#'
#' @family plot_pmx
#' @return ggplot2 object
plot_pmx.pmx_gpar <- function(gpar, p){
  assert_that(is_pmx_gpar(gpar))
  assert_that(is_ggplot(p))
  
  
  ## smoothing
  p <- with(gpar, {
    if(has.smooth){
      p <- p + with(smooth,
                    geom_smooth(se=se, linetype=linetype,
                                         size=size, method=method),na.rm=TRUE)
    }

    if(has.band){
      p <- p +
        with(band,
             geom_hline(yintercept = y, 
                                 linetype = linetype, 
                                 size = size))
    }
    ## labels:title,axis,subtitle...
    p <- p + with(labels, ggplot2::labs(x = x,
                              y = y,
                              title = title,
                              subtitle = subtitle))
    if("legend" %in% names(labels))
      p <- p + with(labels, labs(fill = legend))

    ## limits
    if(!is.null(ranges$y))
      p <- p + scale_y_continuous(limits = ranges$y)
    if(!is.null(ranges$x) && !discrete)
      p <- p + scale_x_continuous(limits = ranges$x)
    
    ## if(scale_log=="x") p <- p + scale_x_log10()
    ## if(scale_log=="y") p <- p + scale_y_log10()
    
    ## theming
    p <- p + pmx_theme()
    ## draft layer
    if(is.draft){
       p <- p + with(draft, add_draft(label, size, color,x,y))
    }
    p
  })
  p

}

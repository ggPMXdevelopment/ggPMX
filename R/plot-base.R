

#' The ggPMX base plot function
#'
#'
#' This function should be called internally by other plots to set
#' general settings like , smoothing, add band, labelling, theming,...
#' @param gpar object of pmx_gpar type
#' @param p plot
#'
#' @return ggplot2 object
plot_pmx.pmx_gpar <- function(gpar, p){
  ## smoothing
  p <- with(gpar, {
    if(has.smooth){
      p <- p + with(smooth,
                    ggplot2::geom_smooth(se=se, linetype=linetype,
                                         size=size, method=method))
    }

    if(has.band){
      p <- p +
        with(band,
             ggplot2::geom_hline(yintercept = y, 
                                 linetype = linetype, 
                                 size = size))
    }
    ## labels:title,axis,subtitle...
    p <- p + with(labels, ggplot2::labs(x = x,
                              y = y,
                              title = title,
                              subtitle = subtitle))
    if("legend" %in% names(labels))
      p <- p + with(labels, ggplot2::labs(fill = legend))

    ## limits
    if(!is.null(ranges$y))
      p <- p + scale_y_continuous(limits = ranges$y)
    if(!is.null(ranges$x) && !discrete)
      p <- p + scale_x_continuous(limits = ranges$x)

    ## theming
    p <- p + pmx_theme()
    ## draft layer
    if(is.draft)
      p <- p + with(draft, add_draft(label, size, color))
    p
  })
  p

}

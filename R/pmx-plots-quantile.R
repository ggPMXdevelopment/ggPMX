
#' Quantile-quantile plots 
#' @name pmx_qq_plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object.
#' \item \code{\link{pmx_update}} function.
#' }
#' \cr \cr 
#' \strong{pmx_qq parameters} \cr

#' @param dname name of dataset to be used
#' @param point \code{list} geom_point parameters.
#' @param reference_line \code{list} geom_abline parameters.  
#' @param facets \code{list} facet_wrap graphical parameters.



#' \cr \cr 
#' \strong{pmx_update parameters} \cr

#' @param filter \code{expression} filter which will be applied to plotting data.
#' @param strat.facet \code{formula} optional stratification parameter by facetting.
#' This split plot by strats(each strat in a facet)
#' @param facets \code{list} facet_wrap parameters.
#' @param strat.color \code{character} optional stratification parameter by grouping.
#' This will split the plot by group (color) of strat.
#' @param trans \code{character} define the transformation to apply on x or y or both variables
#' @param pmxgpar a object of class pmx_gpar possibly the output of the

#' \cr \cr 
#' \strong{pmx_gpar: Shared basic graphics parameters} \cr

#' @param labels \code{list} list containing plot and/or axis labels: title, subtitle, x , y
#' @param axis.title \code{list} containing element_text attributes to customize 
#' the axis title. (similiar to ggplot2 axis.title theme)
#' @param axis.text \code{list} containing element_text attributes to customize 
#' the axis text (similiar to ggplot2 axis.text theme)
#' @param ranges \code{list} limits of x/y ranges
#' @param is.smooth \code{logical} if set to TRUE add smooth layer
#' @param smooth \code{list} geom_smooth graphical/smooting fun parameters
#' @param is.band \code{logical} if TRUE add horizontal band
#' @param band \code{list} horizontal band parameters. geom_hline graphical parameters.
#' @param is.draft \code{logical} if TRUE add draft layer
#' @param draft \code{list} draft layer parameters. geom_text graphical parameters.
#' @param is.identity_line \code{logical} if TRUE add an identity line
#' @param identity_line \code{list}geom_abline graphical parameters.
#' @param scale_x_log10 \code{logical} if TRUE use log10 scale for x axis.
#' @param scale_y_log10 \code{logical} if TRUE use log10 scale for y axis.
#' @param color.scales \code{list} define scales paremeter in case of strat.color \code{\link{pmx_settings}}
#' @return ggplot2 object
#' @example inst/examples/qq.R


NULL



# Quantile-quantile plot of IWRES --------------------------------------------------------------

#' Quantile-quantile plot of IWRES
#' @family qqq
#' @rdname pmx_qq_plot
#' @export


pmx_plot_iwres_qq <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "iwres_qq", params)
  }


#' Quantile-quantile plot of eta variables
#' @family qqq
#' @rdname pmx_qq_plot
#' @export

pmx_plot_eta_qq <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "eta_qq", params)
  }

#' Quantile-quantile plot of NPDE
#' @family qqq
#' @rdname pmx_qq_plot
#' @export
pmx_plot_npde_qq <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "npde_qq", params)
  }




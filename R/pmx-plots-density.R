
#' IWRES density plot

#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{pmx_dens}} pmx density object.
#' \item \code{\link{pmx_update}} function.
#' }
#'
#' \strong{pmx_dens parameters}


#' @param dname \code{character} name of dataset to be used. User can create his own
#' dataset using \code{\link{set_data}} and pass it as dname to be plotted.
#' @param xlim \code{numeric} x axis limits
#' @param var_line \code{list} variable denstiy graphics parameters
#' @param snd_line \code{list} normal denstiy graphics parameters
#' @param vline \code{list} vertical line graphics parameters


#'
#' \strong{pmx_update parameters}

#' @param filter \code{expression} filter which will be applied to plotting data.
#' @param strat.facet \code{formula} optional stratification parameter by facetting.
#' This split plot by strats(each strat in a facet)
#' @param facets \code{list} facet_wrap parameters.
#' @param strat.color \code{character} optional stratification parameter by grouping.
#' This will split the plot by group (color) of strat.
#' @param trans \code{character} define the transformation to apply on x or y or both variables
#' @param pmxgpar a object of class pmx_gpar possibly the output of the

#'
#' \strong{pmx_gpar: Shared basic graphics parameters}

#' @param labels \code{list} list containing plot and/or axis labels: title, subtitle, x , y
#' @param axis.title \code{list} containing element_text attributes to customize
#' the axis title. (similar to ggplot2 axis.title theme)
#' @param axis.text \code{list} containing element_text attributes to customize
#' the axis text (similar to ggplot2 axis.text theme)
#' @param ranges \code{list} limits of x/y ranges
#' @param is.smooth \code{logical} if set to TRUE add smooth layer
#' @param smooth \code{list} geom_smooth graphical/smoothing fun parameters
#' @param is.band \code{logical} if TRUE add horizontal band
#' @param band \code{list} horizontal band parameters. geom_hline graphical parameters.
#' @param is.draft \code{logical} if TRUE add draft layer
#' @param draft \code{list} draft layer parameters. geom_text graphical parameters.
#' @param is.identity_line \code{logical} if TRUE add an identity line
#' @param identity_line \code{list}geom_abline graphical parameters.
#' @param scale_x_log10 \code{logical} if TRUE use log10 scale for x axis.
#' @param scale_y_log10 \code{logical} if TRUE use log10 scale for y axis.
#' @param color.scales \code{list} define scales parameter in case of strat.color \code{\link{pmx_settings}}
#' @param sim_blq \code{logical} if TRUE uses sim_blq as dataset for plotting instead of predictions.
#' @return ggplot2 or list of ggplot2 objects
#' @export
pmx_plot_iwres_dens <- function(ctr, sim_blq, dname, xlim, var_line, snd_line, vline, filter, strat.facet,
                                facets, strat.color, trans, pmxgpar, labels, axis.title,
                                axis.text, ranges, is.smooth, smooth, is.band, band, is.draft,
                                draft, is.identity_line, identity_line, scale_x_log10,
                                scale_y_log10, color.scales,
                                ...) {
  params <- get_params_from_call()
  params$is.smooth <- FALSE
  wrap_pmx_plot_generic(ctr, "iwres_dens", params)
}

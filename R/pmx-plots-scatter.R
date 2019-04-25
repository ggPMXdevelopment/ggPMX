
#' Scatter residual plots
#' @name residual_scatter
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#'
#' \strong{residual parameters}

#' @param point \code{list} geom_point graphical parameters.
#' @param is.hline \code{logical} if TRUE add horizontal line y=0 ( TRUE by default).
#' @param hline \code{list} geom_hline graphical parameters.
#' @param dname \code{character} name of dataset to be used. User can create his own
#' dataset using \code{\link{set_data}} and pass it as dname to be plotted.
#' @param bloq \code{pmxBLOQ} object created by \code{\link{pmx_bloq}}.

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
#' @return ggplot2 object
#' @example inst/examples/residual.R


residual_scatter <-
  function(point, is.hline, hline, dname, bloq, filter, strat.facet,
           facets, strat.color, trans, pmxgpar, labels, axis.title,
           axis.text, ranges, is.smooth, smooth, is.band, band, is.draft,
           draft, is.identity_line, identity_line, scale_x_log10,
           scale_y_log10, color.scales, ...) {}



# scatter residual plots --------------------------------------------------------------

#' DV vs PRED plot
#' @export
#' @family residual
#' @rdname residual_scatter

pmx_plot_dv_pred <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "dv_pred", params)
}


# DV vs IPRED plot --------------------------------------------------------------

#' DV vs IPRED plot
#' @export
#' @family residual
#' @rdname residual_scatter


pmx_plot_dv_ipred <- function(
                              ctr,
                              ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "dv_ipred", params)
}


# IWRES vs IPRED plot --------------------------------------------------------------


#' IWRES vs IPRED plot
#' @export
#' @family residual
#' @rdname residual_scatter


pmx_plot_iwres_ipred <- function(
                                 ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "iwres_ipred", params)
}


# abs IWRES vs IPRED plot --------------------------------------------------------------


#' |IWRES| vs IPRED plot
#' @export
#' @family residual
#' @rdname residual_scatter



pmx_plot_abs_iwres_ipred <- function(
                                     ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "abs_iwres_ipred", params)
}



# IWRES vs TIME plot --------------------------------------------------------------


#' IWRES vs TIME plot
#' @export
#' @family residual
#' @rdname residual_scatter


pmx_plot_iwres_time <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "iwres_time", params)
}


# NPDE vs TIME plot --------------------------------------------------------------

#' NPDE vs TIME plot
#' @export
#' @family residual
#' @rdname residual_scatter


pmx_plot_npde_time <- function(
                               ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "npde_time", params)
}
# NPDE vs PRED plot --------------------------------------------------------------

#' NPDE vs PRED plot
#' @export
#' @family residual
#' @rdname residual_scatter


pmx_plot_npde_pred <- function(
                               ctr,
                               ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "npde_pred", params)
}

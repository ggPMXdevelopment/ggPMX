
#' VPC plot

#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_vpc}} pmx vpc object.
#' \item \code{\link{pmx_update}} function.
#' }
#' \cr \cr 
#' \strong{pmx_vpc parameters} \cr


#' @param type \code{charcater} can be either perecentile or scatter
#' @param idv \code{chracater} individual variable
#' @param obs \code{pmx_vpc_obs} object observation layer \link{pmx_vpc_obs}
#' @param pi \code{pmx_vpc_pi} object percentile layer  \link{pmx_vpc_pi}
#' @param ci \code{pmx_vpc_ci} object confidence interval layer  \link{pmx_vpc_ci}
#' @param rug  \code{pmx_vpc_rug} object rug layer  \link{pmx_vpc_rug}
#' @param bin \code{pmx_vpc_bin} object  \link{pmx_vpc_bin}
#' @param labels \code{list} define title and axis labels
#' @param is.legend \code{logical} if TRUE add legend
#' @param dname added for compatibility with other ggPMX plots
#' @param facets is a list of parameters passed to facet_wrap in case of startification



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
#' @return ggplot2 or list of ggplot2 objects
#' @export
#' @family vpc

#' @example inst/examples/vpc.R


pmx_plot_vpc <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  params$is.smooth <- FALSE
  wrap_pmx_plot_generic(ctr, "pmx_vpc", params)
}

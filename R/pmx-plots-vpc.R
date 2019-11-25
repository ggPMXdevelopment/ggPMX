
#' VPC plot
#'
#' @details
#' You can use \link{pmx_vpc_bin} to set the bin parameters. In case of stratification,
#' binning can be different for each strat level (case \code{within_strat} equal to FALSE).
#'


#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{pmx_vpc}} pmx vpc object.
#' \item \code{\link{pmx_update}} function.
#' }
#'
#' \strong{pmx_vpc parameters}


#' @param type \code{charcater} can be either percentile or scatter
#' @param idv \code{chracater} individual variable
#' @param obs \code{pmx_vpc_obs} object observation layer \link{pmx_vpc_obs}
#' @param pi \code{pmx_vpc_pi} object percentile layer  \link{pmx_vpc_pi}
#' @param ci \code{pmx_vpc_ci} object confidence interval layer  \link{pmx_vpc_ci}
#' @param rug  \code{pmx_vpc_rug} object rug layer  \link{pmx_vpc_rug}
#' @param bin \code{pmx_vpc_bin} object  \link{pmx_vpc_bin}
#' @param is.legend \code{logical} if TRUE add legend
#' @param is.footnote \code{logical} if TRUE add footnote
#' @param dname added for compatibility with other ggPMX plots



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
#' @return ggplot2 or list of ggplot2 objects
#' @export
#' @family vpc

#' @example inst/examples/vpc.R
#'


pmx_plot_vpc <-
  function(ctr, type, idv, obs, pi, ci, rug, bin, is.legend, dname, filter,
             strat.facet, facets, strat.color, trans, pmxgpar, labels,
             axis.title, axis.text, ranges, is.smooth, smooth, is.band,
             band, is.draft, draft, is.identity_line, identity_line,
             scale_x_log10, scale_y_log10, color.scales, is.footnote,...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    params$is.smooth <- FALSE
    wrap_pmx_plot_generic(ctr, "pmx_vpc", params)
  }

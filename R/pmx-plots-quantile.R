
#' Quantile-quantile plots
#' @name pmx_qq_plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object.
#' \item \code{\link{pmx_update}} function.
#' }
#'
#' \strong{pmx_qq parameters}

#' @param dname name of dataset to be used
#' @param point \code{list} geom_point parameters.
#' @param is.reference_line \code{logical} if TRUE add reference line to the plot
#' @param reference_line \code{list} geom_abline parameters.
#' @param is.shrink \code{logical} if TRUE add shrinkage to the plot
#' @param shrink \code{pmxShrinkClass} shrinkage graphical parameter or
#'   \code{list} coercible into one
#' @param is.hline \code{logical} if TRUE add horizontal line y=0 ( TRUE by default)
#' @param hline \code{list} geom hline graphical parameters
#' @param is.vline \code{logical} if TRUE add vertical line x=0 ( TRUE by default)
#' @param vline \code{list} geom vline graphical parameters
#'
#' \strong{pmx_update parameters}


#' @param filter \code{expression} filter which will be applied to plotting data.
#' @param strat.facet \code{formula} optional stratification parameter by facetting.
#' This split plot by strats(each strat in a facet)
#' @param facets \code{list} facet_wrap parameters.
#' @param strat.color \code{character} optional stratification parameter by grouping.
#' This will split the plot by group (color) of strat.
#' @param trans \code{character} define the transformation to apply on x or y or both variables
#' @param pmxgpar an object of class pmx_gpar
#'

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
#' @example inst/examples/qq.R
pmx_qq_plot <-
  function(dname, point, is.reference_line, reference_line, is.shrink,
             shrink, is.hline, hline, is.vline, vline, filter, strat.facet, facets,
             strat.color, trans, pmxgpar, labels, axis.title, axis.text,
             ranges, is.smooth, smooth, is.band, band, is.draft, draft,
             is.identity_line, identity_line, scale_x_log10, scale_y_log10,
             color.scales, ...) {}



# Quantile-quantile plot of IWRES --------------------------------------------------------------

#' Quantile-quantile plot of IWRES
#' @family qqq
#' @rdname pmx_qq_plot
#' @export
pmx_plot_iwres_qq <-
  function(ctr,
             ...) {
    params <- get_params_from_call()
    wrap_pmx_plot_generic(ctr, "iwres_qq", params)
  }


#' Quantile-quantile plot of eta variables
#' @family qqq
#' @rdname pmx_qq_plot
#' @export

pmx_plot_eta_qq <-
  function(ctr,
             ...) {
    params <- get_params_from_call() %>% append(list(is.hline=FALSE))
    wrap_pmx_plot_generic(ctr, "eta_qq", params)
  }

#' Quantile-quantile plot of NPDE
#' @family qqq
#' @rdname pmx_qq_plot
#' @export
pmx_plot_npde_qq <-
  function(ctr,
             ...) {
    params <- get_params_from_call()
    wrap_pmx_plot_generic(ctr, "npde_qq", params)
  }


#' Quantile-quantile plot of NPD
#' @family qqq
#' @rdname pmx_qq_plot
#' @export
pmx_plot_npd_qq <-
  function(ctr,
             ...) {
    params <- get_params_from_call()
    wrap_pmx_plot_generic(ctr, "npd_qq", params)
  }


#' Quantile-quantile plot of CWRES
#' @family qqq
#' @rdname pmx_qq_plot
#' @export
pmx_plot_cwres_qq <-
  function(ctr,
             ...) {
    params <- get_params_from_call()
    wrap_pmx_plot_generic(ctr, "cwres_qq", params)
  }

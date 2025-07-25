
#' Eta Covariates plots
#' @name eta_cov_plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{eta_cov}} generic object for eta/covariates plots.
#' \item \code{\link{pmx_update}} function.
#' }
#'
#' \strong{eta_cov parameters}

#' @param dname \code{character} name of dataset to be used
#' @param show.correl \code{logical} if TRUE add correlation to the plot
#' @param correl \code{list} correl geom text graphical parameter
#' @param point \code{list} geom point graphical parameter
#' @param facets \code{list} facetting graphical parameter
#' @param covariates list of selected covariates and custom labels, e.g. \code{pmx_cov(values = list("SEX"), labels = list("Sex"))} \code{list} \code{pmxCOVObject} \code{\link{pmx_cov}}


#'
#' \strong{pmx_update parameters}

#' @param filter \code{expression} filter which will be applied to plotting data.
#' @param strat.facet \code{formula} optional stratification parameter by facetting.
#' This split plot by strats(each strat in a facet)
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
#' @example inst/examples/eta_cov.R
dummy <-
  function(dname, show.correl, correl, point, facets, covariates, filter,
             strat.facet, strat.color, trans, pmxgpar, labels, axis.title,
             axis.text, ranges, is.smooth, smooth, is.band, band, is.draft,
             draft, is.identity_line, identity_line, scale_x_log10,
             scale_y_log10, color.scales) {

  }




# eta cats plot --------------------------------------------------------------

#' Relationships between (ETA) and categorical covariates
#' @family eta_cov_plot
#' @return ggplot2 object
#' @rdname eta_cov_plot
#' @export

pmx_plot_eta_cats <-
  function(ctr,
             ...) {
    params <- get_params_from_call()
    wrap_pmx_plot_generic(ctr, "eta_cats", params)
  }

# eta conts plot --------------------------------------------------------------

#' Relationships between (ETA) and continuous covariates
#' @family eta_cov_plot
#' @return ggplot2 object
#' @rdname eta_cov_plot
#' @export
pmx_plot_eta_conts <-
  function(ctr,
             ...) {
    params <- list(...)
    wrap_pmx_plot_generic(ctr, "eta_conts", params)
  }

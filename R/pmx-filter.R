#' filter data in a pmx controller
#'
#' @param ctr A controller. An object of 'pmxClass'
#' @param data_set A data_set within the controller to apply a filter to.
#' @param pmx_exp A filter expression
#'
#' @return Returns a pmx controller with a filtered data set.
#' @export
#' @examples
#' \donttest{
#' ## example of global filter
#' ctr <- theophylline()
#' ctr %>% pmx_filter(data_set = "prediction", ID == 5 & TIME < 2)
#' ctr %>% get_data("prediction")
#' }
pmx_filter <-
  function(ctr, data_set = c(
               "estimates",
               "predictions",
               "eta",
               "finegrid",
               "shrink",
               "input",
               "individual",
               "sim_blq"
             ), pmx_exp) {
    assert_that(is_pmxclass(ctr))
    data_set <- match.arg(data_set)
    assert_that(is_language_or_string(substitute(pmx_exp)))
    if (is_string(substitute(pmx_exp))) {
      pmx_exp <- expression(pmx_exp)
    }
    if (!is.null(substitute(pmx_exp))) {
      if (data_set == "individual") data_set <- "IND"

      filter <- deparse(substitute(pmx_exp))
      filter <- local_filter(filter)
      oldData <- ctr[["data"]][[data_set]]
      ctr[["data"]][[data_set]] <- filter(oldData)
      ## update all plots after global filtering
      for (nn in ctr %>% plot_names()) {
        ctr %>% pmx_update(pname = nn)
      }
    }

    ctr
  }

#' Update plot object
#'
#' @param ctr  \code{pmxClass} controller object
#' @param pname character the plot name to update
#' @param filter optional filter which will be applied to plotting data
#' @param strat.facet \code{formula} optional stratification parameter
#' @param strat.color \code{character} optional stratification parameter
#' @param trans \code{character} define the transformation to apply on x or y or both variables
#' @param ... others graphical parameters given to set the plot
#' @param  pmxgpar a object of class pmx_gpar possibly the output of the
##' @param color.scales \code{list} can be used with strat.color to set scale_color_manual

#' \code{\link{pmx_gpar}} function.
#'
#' @family pmxclass
#' @return controller object with the plot updated
#' @export
#' @details
#'
#' \strong{trans} is a transformation that user can apply to x, or y coordinates. The transformation
#' is applied to the data before the plotting. This gives more felxiblilty to the user and also conserves
#' all static positions like annotations ( draft specially)
#'
#' For example:
#'
#'  var_x apply variance to x coordinates the variance function
#'
#'  var_xy apply variance to both
#' This mechanism is applied internally to scale log.

pmx_update <- function(ctr, pname, strat.color = NULL, strat.facet = NULL,
                       color.scales = NULL,
                       filter = NULL, trans = NULL, ..., pmxgpar = NULL) {
  assert_that(is_pmxclass(ctr))
  assert_that(is_string(pname))
  ## assert_that(is_string_or_null(strat.color))
  assert_that(is_string_or_formula_or_null(strat.facet))

  ## filtering
  if (!is.null(substitute(filter))) {
    filter <- deparse(substitute(filter))
    filter <- local_filter(filter)
  }

  ctr$update_plot(
    pname,
    strat.color = strat.color,
    strat.facet = strat.facet,
    filter = filter,
    trans = trans,
    color.scales = color.scales,
    ...,
    pmxgpar = pmxgpar
  )
}


pmx_update_plot <- function(self, private, pname, strat.facet,
                            strat.color, filter = NULL, trans = NULL,
                            color.scales, ..., pmxgpar) {
  # assertthat::assert_that(isnullOrPmxgpar(pmxgpar))
  x <- private$.plots_configs[[pname]]
  old_class <- class(x)
  old_class_gp <- class(x$gp)

  ## update graphical parameters
  if (!is.null(pmxgpar)) x <- l_left_join(x, pmxgpar)
  
  if ((!is.null(self[["settings"]])) 
      && (length(attributes(self[["settings"]])) > 2)) {
        x <- l_left_join(x, attributes(self[["settings"]]))
  }
  
  newopts <- list(...)

  if (length(newopts) > 0) {
    hl <- newopts[names(newopts) %in% unique(c(names(x), "shrink"))]
    gpl <- newopts[!names(newopts) %in% unique(c(names(x), "shrink"))]
    if ("labels" %in% names(newopts)) gpl$labels <- l_left_join(x$gp$labels, newopts[["labels"]])
    hl$gp <- gpl
    x <- l_left_join(x, hl)
  }

  if (!is.null(filter)) x[["filter"]] <- filter
  ## transformation
  if (!is.null(trans)) x[["trans"]] <- trans
  ## stratification
  x[["strat.color"]] <- strat.color
  x[["strat.facet"]] <- strat.facet
  if (!is.null(color.scales)) {
    x$gp[["color.scales"]] <- color.scales
  }


  class(x$gp) <- old_class_gp
  class(x) <- old_class

  self$remove_plot(pname)
  self$add_plot(x, pname)
}

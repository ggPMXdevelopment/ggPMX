

add_footnote <- function(pp, pname, save_dir) {
  plot_file <- file.path(save_dir, "ggpmx_GOF", pname)
  footnote <- sprintf("Source: %s", plot_file)
  ## message("footnote is :" , footnote)
  if (nchar(footnote) > 45) {
    fns <- strsplit(footnote, "/")[[1]]
    term1 <- do.call(file.path, as.list(fns[cumsum(nchar(fns)) < 45]))
    term2 <- do.call(file.path, as.list(fns[cumsum(nchar(fns)) >= 45]))
    footnote <- paste(paste0(term1, "/"), term2, sep = "\n")
  }
  pp <- pp + labs(caption = footnote)
  pp
}



#' Individual plot

#' @param ctr pmx controller
#' @param npage \code{integer} page(s) to display , set npage to NULL
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{individual}} generic object for individual plots.
#' \item \code{\link{pmx_update}} function.
#' }
#' \strong{individual parameters}
#' @param dname \code{character} name of dataset to be used. User can create his own
#' dataset using \code{\link{set_data}} and pass it as dname to be plotted.
#' @param pred_line \code{list} some ipred line geom properties aesthetics
#' @param ipred_line \code{list} some pred line geom properties aesthetics
#' @param point \code{list} some point geom properties aesthetics
#' @param is.legend \code{logical} if TRUE add a legend
#' @param use.finegrid \code{logical} if FALSE use predictions data set
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
#' @param print  \code{logical} if TRUE the ouptut will be a print not a ggplot2. This
#' is useful for rmarkdwon output to avoid verbose list index print.
#' @return ggplot2 or list of ggplot2 objects
#' @example inst/examples/individual.R
#' @export
pmx_plot_individual <-
  function(
             ctr,
             npage = 1,
             print = FALSE,
             dname, pred_line, ipred_line, point, is.legend, use.finegrid,
             bloq, filter, strat.facet, facets, strat.color, trans,
             pmxgpar, labels, axis.title, axis.text, ranges, is.smooth,
             smooth, is.band, band, is.draft, draft, is.identity_line,
             identity_line, scale_x_log10, scale_y_log10, color.scales,
             ...) {
    stopifnot(is_pmxclass(ctr))
    if (!"individual" %in% (ctr %>% plot_names())) {
      return(NULL)
    }
    cctr <- pmx_copy(ctr, ...)
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    params <- lang_to_expr(params)

    defaults_ <- ctr$config$plots[[toupper("individual")]]

    if (!exists("bloq", params) && !is.null(ctr$bloq)) {
      defaults_[["bloq"]] <- ctr$bloq
    }
    params <- l_left_join(defaults_, params)
    params$pname <- "individual"
    params$ctr <- cctr


    do.call("pmx_update", params)
    p <- if (is.null(npage)) {
      cctr %>% get_plot("individual")
    } else {
      cctr %>% get_plot("individual", npage)
    }

    cctr %>% pmx_warnings("MISSING_FINEGRID")



    if (cctr$footnote) {
      if (!inherits(p, "ggplot")) {
        p <- Map(
          function(p, id) {
            ctr$enqueue_plot("indiv")
            add_footnote(p, ctr$plot_file_name, cctr$save_dir)
          },
          p, seq_along(p)
        )
      } else {
        ctr$enqueue_plot("indiv")
        p <- add_footnote(p, ctr$plot_file_name, cctr$save_dir)
      }
    }

    rm(cctr)

    if (print) {
      if (is.list(p)) invisible(lapply(p, print)) else p
    } else {
      p
    }
  }

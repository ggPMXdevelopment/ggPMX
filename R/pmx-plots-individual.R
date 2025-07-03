add_footnote <- function(pp, pname, save_dir) {
  plot_file <- file.path(save_dir, "ggpmx_GOF", pname)
  footnote <- sprintf("Source: %s", plot_file)

  # Establish maximum width of footnote (num characters):
  max_width <- ifelse(grepl("vpc", pname, fixed = TRUE), 40, 60)
  pp + labs(caption=split_footnote(footnote, max_width))
}


#' Split footnote if it exceeds maximum width

#' @param s \code{character} the footnoote text
#' @param n \code{integer} the maximum width of a footnote
#' @returns character with split input character
#' @examples
#' split_footnote("Source: /tmp/mylongdirectory", 10)
#' @noRd
split_footnote <- function(s, n) {
  stopifnot(
    is.character(s),
    length(s) == 1,
    is.numeric(n),
    length(n) == 1,
    n > 1
  )
  # Splitting footnote into multiple lines if exceeding max width:
  if(nchar(s) <= n) return(s)

  # Case where a single segment between slashes exceeds max width:
  if(grepl(paste0("[^/]{", n, "}"), s)){
    # Split at fixed intervals:
    paste(strsplit(s, paste0("(?<=.{", n, "})"), perl=TRUE)[[1]], collapse="\n")
  } else {
    # Split at forward slashes (greedy):
    gsub(paste0("(.{1,", n, "})(?=/)"), "\\1\n", s, perl=TRUE)
  }
}



#' Individual plot

#' @param ctr pmx controller
#' @param which_pages \code{integer} page(s) to display, or \code{character}
#' "all" to display all pages (argument previously called npage, now deprecated)
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
             which_pages = 1L,
             print = FALSE,
             dname, pred_line, ipred_line, point, is.legend, use.finegrid,
             bloq, filter, strat.facet, facets, strat.color, trans,
             pmxgpar, labels, axis.title, axis.text, ranges, is.smooth,
             smooth, is.band, band, is.draft, draft, is.identity_line,
             identity_line, scale_x_log10, scale_y_log10, color.scales,
             ...) {
    stopifnot(is_pmxclass(ctr))
    if ("npage" %in% names(list(...))) {
       warning("The argument npage is obsolete, please use which_pages instead")
    }
    if (!"individual" %in% (ctr %>% plot_names())) {
      return(NULL)
    }

    cctr <- pmx_copy(ctr, ...)

    params <- get_params_from_call()
    defaults_ <- ctr$config$plots[[toupper("individual")]]

    if (!exists("bloq", params) && !is.null(ctr$bloq)) {
      defaults_[["bloq"]] <- ctr$bloq
    }
    params <- l_left_join(defaults_, params)
    params$pname <- "individual"
    params$ctr <- cctr


    do.call("pmx_update", params)
    p <- if ((length(which_pages) == 1L) && (which_pages == "all")) {
      cctr %>% get_plot("individual")
    } else {
      cctr %>% get_plot("individual", which_pages)
    }

    cctr %>% pmx_warnings("MISSING_FINEGRID")



    if (cctr$footnote) {
      if (!is_ggplot(p)) {
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
      if (is.list(p) & (!is_ggplot(p))) {
        invisible(lapply(p, print))
      } else {
        invisible(print(p))
      }
    } else {
      p
    }
  }

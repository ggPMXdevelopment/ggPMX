
#' This function can be used to obtain individual prediction and compare with observed data and population prediction
#' for each individual separately
#'
#' @param labels  plot texts. labels, axis,
#' @param facets list facets settings nrow/ncol
#' @param dname name of dataset to be used
#' @param pred_line \code{list} some ipred line geom properties aesthetics
#' @param ipred_line \code{list} some pred line geom properties aesthetics
#' @param point \code{list} some point geom properties aesthetics
#' @param is.legend \code{logical} if TRUE add a legend
#' @param use.finegrid \code{logical} if FALSE use predictions data set
#' @param bloq \code{pmxBLOQ} object created by \code{\link{pmx_bloq}}
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#'
#' @return individual fit object
#' @family plot_pmx
#' @seealso \code{\link{plot_pmx.individual}}


individual <- function(labels,
                       facets = NULL,
                       dname = NULL,
                       ipred_line = NULL,
                       pred_line = NULL,
                       point = NULL,
                       bloq = NULL,
                       is.legend,
                       use.finegrid,
                       ...) {
  assert_that(is_list(facets))
  assert_that(is_string_or_null(dname))
  assert_that(is_list(labels))


  if (!use.finegrid) dname <- "predictions"

  structure(list(
    ptype = "IND",
    strat = TRUE,
    is.legend = is.legend,
    use.finegrid = use.finegrid,
    dname = dname,
    aess = list(x = "TIME", y1 = "PRED", y2 = "IPRED"),
    labels = labels,
    point = point,
    ipred_line = ipred_line,
    pred_line = pred_line,
    facets = facets,
    bloq = bloq,
    gp = pmx_gpar(labels = labels, is.legend = is.legend, ...)
  ), class = c("individual", "pmx_gpar"))
}



#' This function can be used to plot individual prediction and compare with observed data and population prediction
#' for each individual separately

#' @param x individual object
#' @param dx data set
#' @param ... not used for the moment
#'
#' @return a list of ggplot2
#' @export
#' @import ggplot2
#' @import data.table
#' @family plot_pmx
#'
plot_pmx.individual <-
  function(x, dx, ...) {
    ID <- NULL
    ## plot
    if (x$dname == "predictions") cat("USE predictions data set \n")
    strat.facet <- x[["strat.facet"]]
    strat.color <- x[["strat.color"]]

    wrap.formula <- if (!is.null(strat.facet)) {
      wrap_formula(strat.facet, "ID")
    } else {
      formula("~ID")
    }
    get_page <- with(x, {
      p_point <- if (!is.null(point)) {
        point$data <- if (is.null(bloq)) {
          input
        } else {
          input[!get(bloq$cens) %in% c(1, -1)]
        }
        point.shape <- point$shape
        point$shape <- NULL
        do.call(geom_point, point)
      }
      values.lty <- c()
      p_ipred <- if (!is.null(ipred_line)) {
        ipred_line.lty <- ipred_line$linetype
        ipred_line$linetype <- NULL
        ipred_line$mapping <- aes(y = IPRED, linetype = "individual predictions")
        values.lty <- c("individual predictions" = as.integer(ipred_line.lty))
        do.call(geom_line, ipred_line)
      }
      p_pred <- if (!is.null(pred_line)) {
        pred_line.lty <- pred_line$linetype
        pred_line$linetype <- NULL
        pred_line$mapping <- aes(y = PRED, linetype = "population predictions")
        values.lty <- c(values.lty, "population predictions" = as.integer(pred_line.lty))
        do.call(geom_line, pred_line)
      }


      p_bloq <- if (!is.null(bloq)) {
        bloq$data <- x$input[get(bloq$cens) != 0]
        if (length(bloq$data$ID) > 0){
          ## While cens may be in the dataset, all the data in the fit may be uncensored
          if (bloq$limit %in% names(bloq$data)) {
            bloq$data[!is.na(get(bloq$limit)), "y_end" := as.numeric(get(bloq$limit))]
          }
          bloq$mapping <-
            aes_string(
              xend = "TIME",
              yend = "y_end"
            )
          bloq$cens <- bloq$limit <- NULL
          do.call(geom_segment, bloq)
        }
      }

      p <- ggplot(dx, aes(TIME, DV, shape = "observations")) +
        p_point + p_ipred + p_pred + p_bloq


      if (is.legend) {
        p <- p + scale_linetype_manual("", values = values.lty) +
          guides(linetype = guide_legend(keywidth = 2)) +
          scale_shape_manual("", values = c("observations" = point.shape))
        gp$is.legend <- is.legend
      }

      p <- plot_pmx(gp, p)

      ## split pages
      npages <- ceiling(with(
        facets,
        length(unique(dx$ID)) / nrow / ncol
      ))

      function(i) {
        res <- list()
        if (is.null(i)) i <- seq_len(npages)
        i <- intersect(i, seq_len(npages))
        res <- lapply(i, function(x) {
          facets$page <- x
          facets$facets <- wrap.formula
          if (is.null(facets$labeller)) {
            facets$labeller <- labeller(ID = function(x) sprintf("ID: %s", x))
          }
          p + do.call(facet_wrap_paginate, facets)
        })
        if (length(res) == 1) res[[1]] else res
      }
    })

    get_page
  }

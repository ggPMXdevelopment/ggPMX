
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



get_invcolor <- function(color){
  if (length(color) > 1) color[2]
  else "red"
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
    dx$maxValue <- 0
    ## plot
    if (x$dname == "predictions") cat("USE predictions data set \n")

    strat.facet <- x[["strat.facet"]]

    # dropping any rows with NA in the faceting column
    if (!is.null(strat.facet)) {
      faceting_column <- {
        if (inherits(strat.facet, "formula")) {
          # converting to character required by tidyr::drop_na
          faceting_column <- sub("...", "", toString(strat.facet))
        } else {strat.facet}
      }

      dx <- tidyr::drop_na(dx, tidyr::all_of(faceting_column))
      x[["dx"]] <- tidyr::drop_na(x[["dx"]], tidyr::all_of(faceting_column))
    }

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
        max_y <- aggregate(TIME ~ ID, data=dx, max)
        colnames(max_y) <- c("ID", "maxValue")
        dx <- base::merge(dx, max_y, by="ID", all.x = TRUE)
        # Rounding because "predictions" data are rounded:
        dx$isobserv <- with(dx, round(TIME) <= maxValue)
        point$data <- base::merge(point$data, max_y, by="ID")
        # Rounding because "predictions" data are rounded:
        point$data$isobserv <-
          ifelse(round(point$data$TIME, 4) <= round(point$data$maxValue, 4), " observed", "censored")
        points <- copy(point)
        points$colour <- NULL
        do.call(geom_point, points)
      }

      p_bloq <- if (!is.null(bloq)) {
        bloq$data <- x$input[get(bloq$cens) != 0]
        if (length(bloq$data$ID) > 0) {
          ## While cens may be in the dataset, all the data in the fit may be uncensored
          if (bloq$limit %in% names(bloq$data)) {
            bloq$data[!is.na(get(bloq$limit)), "y_end" := as.numeric(get(bloq$limit))]
            bloq$mapping <-
            aes(
              xend = .data$TIME,
              yend = .data$y_end
            )
            bloq$cens <- bloq$limit <- bloq$size <- NULL
          do.call(geom_segment, bloq)
          }
        }
      }
      if (!is.null(point)) {
        n <- ifelse(any(point$data$isobserv == "censored"), 3,
                    ifelse(length(point$data$isobserv) == 0L, 1, 2))
        linetype_values <- c(rep("solid", n), "dashed")
        if (any(point$data$isobserv == "censored")) {
          linetype_labels <- c(" observed",
                               "censored",
                               "individual predictions",
                               "population predictions")
        } else if (length(point$data$isobserv) == 0L) {
          linetype_labels <- c("individual predictions",
                               "population predictions")
        } else {
          linetype_labels <- c(" observed",
                               "individual predictions",
                               "population predictions")
        }
      } else {
        n <- 2
        linetype_labels <- c(" observed",
                             "individual predictions",
                             "population predictions")
      }


      shape_values <- c(rep(point.shape, n + 1))
      shape_values_leg <- c(rep(point.shape, n - 1), rep(20, 2))
      linewidth_values <- c(rep(1, n - 1), ipred_line$linewidth, pred_line$linewidth)
      if (any(point$data$isobserv == "censored"))
        colour_values <- c(point$colour[1],
                           get_invcolor(point$colour),
                           ipred_line$colour,
                           pred_line$colour)
      else
        colour_values <- c(point$colour[1],
                           ipred_line$colour,
                           pred_line$colour)
      keywidth_values <- c(rep(0, n - 1), rep(2, 2))

      p <- ggplot(dx, aes(TIME, DV, shape = isobserv, colour = isobserv)) +
        p_point +
        geom_line(aes(y=.data$IPRED, linetype = "individual predictions",
                      colour = "individual predictions"),
                  linewidth=ipred_line[["linewidth"]]) +
        geom_line(aes(y = .data$PRED, linetype = "population predictions",
                    colour = "population predictions"),
                linewidth=pred_line[["linewidth"]]) +
        scale_linetype_manual(
          values = setNames(
            linetype_values,
            linetype_labels
          ),
          guide = "none"
        ) +
        scale_shape_manual(
          values = setNames(
            shape_values,
            linetype_labels
          ),
          guide = "none"
        ) +
        scale_colour_manual(
          values = setNames(
            colour_values,
            linetype_labels
          ),
          guide = guide_legend(
            override.aes = list(
              linetype = linetype_values,
              shape = shape_values_leg,
              linewidth = linewidth_values
            ),
            title = NULL,
            keywidth = keywidth_values
          )
        ) +
        p_bloq

      gp$is.legend <- is.legend

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
          p + do.call(ggforce::facet_wrap_paginate, facets)
        })
        if (length(res) == 1) res[[1]] else res
      }
    })

    get_page
  }



#' This creates an ETA covriance matrix which can be used to define the co-relation between the parameters and
#' its shrinkage..
#'
#' @param labels list of texts/titles used within the plot
#' @param type box for cats or conts
#' @param dname name of dataset to be used
#' @param show.correl \code{logical} if TRUE add correlation to the plot
#' @param correl \code{list} correl geom text graphical parameter
#' @param point \code{list} geom point graphical parameter
#' @param facets \code{list} facetting graphical parameter
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return \code{eta_cov} object
#' @family plot_pmx
#' @details
#'
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default  "EBE vs. covariates"}
#' \item {\strong{x:}} {x axis label default to "Etas"}
#' \item {\strong{y:}} {y axis label default to empty}
#' }
#'
#' @export
eta_cov <- function(
                    labels,
                    type = c("cats", "conts"),
                    dname = NULL,
                    show.correl=TRUE,
                    correl=list(size = 5, colour = "blue"),
                    facets=list(scales = "free"),
                    point = list(colour = "gray"),
                    ...) {
  type <- match.arg(type)
  assert_that(is_string_or_null(dname))
  if (is.null(dname)) dname <- "eta"


  if (missing(labels)) {
    labels <- list(
      title = "EBE vs. covariates",
      x = "",
      y = ""
    )
  }
  assert_that(is_list(labels))
  labels$subtitle <- ""
  structure(list(
    ptype = "ETA_COV",
    strat = FALSE,
    dname = dname,
    type = type,
    show.correl = show.correl,
    correl = correl,
    facets = facets,
    smooth = smooth,
    point = point,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      ...
    )
  ), class = c("eta_cov", "pmx_gpar"))
}

















#' This plots an ETA covriance matrix which can be used to define the co-relation between the parameters and
#' its shrinkage
#'
#' @param x eta_cov object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggplot2 plot
#' @export
#' @seealso \code{\link{eta_cov}}
#' @family plot_pmx
#' @import ggplot2
#' @importFrom stats cor
#'
plot_pmx.eta_cov <- function(x, dx, ...) {
  p <- if (x$type == "cats") {
    x$gp$is.smooth <- FALSE
    cats <- x[["cats"]]
    if (all(nzchar(x[["cats"]]))) {
      dx.cats <- dx[, c(cats, "VALUE", "EFFECT"), with = FALSE]
      ggplot(melt(dx.cats, measure.vars = cats)) +
        geom_boxplot(aes_string(x = "value", y = "VALUE")) +
        facet_grid(as.formula("EFFECT~variable"), scales = "free")
    }
  } else {
    value <- variable <- NULL
    conts <- x[["conts"]]
    if (all(nzchar(x[["conts"]]))) {
      dx.conts <- dx[, c(conts, "VALUE", "EFFECT"), with = FALSE]
      dx.conts <- melt(dx.conts, id = c("VALUE", "EFFECT"))
      x$facets$facets <- as.formula("EFFECT~variable")
      p <- ggplot(dx.conts, aes_string(x = "value", y = "VALUE")) +
        do.call(geom_point, x$point) +
        ## do.call(geom_smooth, x$smooth) +
        do.call(facet_grid, x$facets)
      if (x$show.correl) {
        df_cor <-
          dx.conts[
            , "corr" := round(cor(get("value"), get("VALUE"), use = "na.or.complete"), 3)
            , "EFFECT,variable"
          ]
        p <- p +
          with(
            x$correl,
            geom_text(
              data = df_cor, aes_string(label = paste("correlation=", "corr")),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2, size = size, colour = colour
            )
          )
      }
      p
    }
  }
  if (!is.null(p)) plot_pmx(x$gp, p)
}

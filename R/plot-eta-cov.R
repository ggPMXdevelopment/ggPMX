
#' Select/Map covariates using human labels
#' @param values \code{list} of covariates to use to  create the plot
#' @param labels \code{list} of covariates facets labels
#' @details
#'
#' In case of `pmx_plot_eta_cats` and `pmx_plot_eta_conts` you can customize the covariates
#' and covaraites labels using `pmx_cov`.
#'
#' @return \code{pmxCOVObject} object
#' @export

pmx_cov <-
  function(values, labels = NULL) {
    assert_that(is_list(values))
    assert_that(is_list_or_null(labels))
    if (missing(labels) || is.null(labels)) labels <- values
    assert_that(length(values) == length(labels))

    structure(
      list(
        values = values,
        labels = labels
      ),
      class = c("pmxCOVObject")
    )
  }


is_pmxcov <- function(x)
  inherits(x, "pmxCOVObject") || is.null(x)
#' This creates an ETA covariance matrix which can be used to define the co-relation between the parameters and
#' its shrinkage..
#'
#' @param labels list of texts/titles used within the plot
#' @param type box for cats or conts
#' @param dname name of dataset to be used
#' @param show.correl \code{logical} if TRUE add correlation to the plot
#' @param correl \code{list} correl geom text graphical parameter
#' @param point \code{list} geom point graphical parameter
#' @param facets \code{list} facetting graphical parameter
#' @param covariates \code{pmxCOVObject} \code{\link{pmx_cov}}
#' @param is.strat.color \code{logical} if `TRUE` use a different color for the spline stratification.
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return \code{eta_cov} object
#' @family plot_pmx
#' @details
#'
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item \strong{title:} plot title default  "EBE vs. covariates"
#' \item \strong{x:} x axis label default to "Etas"
#' \item \strong{y:} y axis label default to empty
#' }
eta_cov <- function(
                    labels,
                    type = c("cats", "conts"),
                    dname = NULL,
                    show.correl = TRUE,
                    correl = NULL,
                    facets = NULL,
                    point = NULL,
                    covariates = NULL,
                    is.strat.color = FALSE,
                    ...) {
  type <- match.arg(type)
  assert_that(is_string_or_null(dname))
  assert_that(is_pmxcov(covariates))

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
    strat = TRUE,
    dname = dname,
    type = type,
    show.correl = show.correl,
    correl = correl,
    facets = facets,
    point = point,
    covariates = covariates,
    is.strat.color = is.strat.color,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      ...
    )
  ), class = c("eta_cov", "pmx_gpar"))
}

#' This plots an ETA covariance matrix which can be used to define the co-relation between the parameters and
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
  assert_that(is_pmxcov(x$covariates))

  p <- if (x$type == "cats") {
    x$gp$is.smooth <- FALSE
    cats <- x[["cats"]]
    if (all(nzchar(x[["cats"]]))) {
      dx.cats <- dx[, c(cats, "VALUE", "EFFECT"), with = FALSE]
      dx.cats <- melt(dx.cats, measure.vars = cats)
      if (!is.null(x$covariates)) {
        dx.cats <-
          with(
            x$covariates,
            dx.cats[variable %in% values][
             ,
              variable := factor(variable, levels = values, labels = labels)
            ]
          )
      }
      if (x$is.strat.color) {
        if(length(cats) > 1) {
          dx.cats <- dx.cats[, var_val = paste0(variable, value)]
          boxplot_layers <- geom_boxplot(aes(x = .data$value, y = .data$VALUE, fill = .data$var_val))
        }
        else boxplot_layers <- geom_boxplot(aes(x = .data$value, y = .data$VALUE, fill = .data$value))
      }
      else {
        boxplot_layers <- geom_boxplot(aes(x = .data$value, y = .data$VALUE))
      }

      ggplot(dx.cats) +
        boxplot_layers +
        facet_grid(stats::as.formula("EFFECT~variable"), scales = "free")
    }
  } else {
    value <- variable <- corr <- corr_exp <- NULL
    conts <- x[["conts"]]
    if (all(nzchar(x[["conts"]]))) {
      dx.conts <- unique(dx[, c(conts, "VALUE", "EFFECT"), with = FALSE])
      dx.conts <- melt(dx.conts, id = c("VALUE", "EFFECT"))
      if (!is.null(x$covariates)) {
        dx.conts <-
          with(
            x$covariates,
            dx.conts[variable %in% values][
              ,
              variable := factor(variable, levels = values, labels = labels)
            ]
          )
      }
      x$facets$rows <- stats::as.formula("EFFECT~variable")
      p <- ggplot(dx.conts, aes(x = .data$value, y = .data$VALUE)) +
        do.call(geom_point, x$point) +
        ## do.call(geom_smooth, x$smooth) +
        do.call(facet_grid, x$facets)
      if (x$show.correl) {
        df_cor <-
          dx.conts[
            , list(corr = round(cor(get("value"), get("VALUE"), use = "na.or.complete"), 3)),
            "EFFECT,variable"
          ]

        corr_eqn <- function(x) {
          eq <- substitute(italic(corr) == a, list(a = x))
          as.character(as.expression(eq))
        }

        df_cor[, corr_exp := corr_eqn(corr), "EFFECT,variable"]

        correl_obj <- list(
          data = df_cor,
          x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2,
          mapping = aes(label = corr_exp), parse = TRUE
        )
        correl_obj <- l_left_join(x$correl, correl_obj)
        p <- p + do.call("geom_text", correl_obj)
      }
      p
    }
  }
  if (!is.null(p)) plot_pmx(x$gp, p)
}

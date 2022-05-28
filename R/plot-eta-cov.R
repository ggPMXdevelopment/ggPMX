

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
#' @param is.hline \code{logical} if TRUE add horizontal line to lower matrix plots
#' @param hline \code{list} geom_hline graphical parameters
#' @param is.jitter \code{logical} if TRUE add jitter operator for points
#' @param jitter list set jitter parameter
#' @param scale string parameter for y axis ("fix", "free", "free_y", "sym")
#' @param is.shrink \code{logical} if TRUE add shrinkage to the plot
#' @param shrink\code{list} shrinkage graphical parameter
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
eta_cov <- function(
                    labels,
                    type = c("cats", "conts"),
                    dname = NULL,
                    show.correl = TRUE,
                    correl = NULL,
                    facets = NULL,
                    point = NULL,
                    is.hline = FALSE,
                    hline = NULL,
                    covariates = NULL,
                    is.strat.color = FALSE,
                    is.jitter = FALSE,
                    jitter = NULL,
                    scale = "free",
                    is.shrink = TRUE,
                    shrink = NULL,
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
    is.hline = is.hline,
    hline = hline,
    covariates = covariates,
    is.strat.color = is.strat.color,
    is.jitter = is.jitter,
    jitter = jitter,
    scale = scale,
    is.shrink = is.shrink,
    shrink = shrink,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      ...
    )
  ), class = c("eta_cov", "pmx_gpar"))
}

check_shrink <- function(shrink) {
  if (is.null(shrink$x))
    shrink$x = -Inf
  if (is.null(shrink$y))
    shrink$y = Inf
  if (is.null(shrink$hjust))
    shrink$hjust = -0.2
  if (is.null(shrink$vjust))
    shrink$vjust = 1.2
  if (is.null(shrink$size))
    shrink$size = 4
  shrink
}

get_shrink_val <- function(shrink.dx, shrink) {
  df_shr <- shrink.dx
  df_shr$SHRINK <-
    sapply(shrink.dx$SHRINK, function (x)
      sprintf("%s%%", round(x * 100)))
  shr_eqn <- function(x) {
    eq <- substitute(italic(shrink) == a, list(a = x))
    as.character(as.expression(eq))
  }
  shrink <- check_shrink(shrink)
  df_shr[, shr_exp := shr_eqn(SHRINK), "EFFECT"]
  shrink_val <- list(
    data = df_shr,
    x = shrink$x,
    y = shrink$y,
    hjust = shrink$hjust,
    vjust = shrink$vjust,
    size = shrink$size,
    mapping = aes(label = shr_exp),
    parse = TRUE
  )
  shrink_val
}

check_jitter <- function(jitter){
  if (is.null(jitter$size))
    jitter$size = 2
  if (is.null(jitter$colour))
    jitter$colour = "black"
  if (is.null(jitter$shape))
    jitter$shape = 20
  if (is.null(jitter$alpha))
    jitter$alpha = 1
  jitter
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
      dx.cats <- unique(dx[, c(cats, "VALUE", "EFFECT"), with = FALSE])
      df <- dx.cats
      dx.cats <- melt(dx.cats, id = c("VALUE", "EFFECT"))
      if (!is.null(x$covariates)) {
        dx.cats <-
          with(
            x$covariates,
            dx.cats[variable %in% values][
             ,
              variable := factor(variable, levels = values, labels = labels)
            ]
          )
       if (all(unlist(x$covariates$values) %in% names(df)))
           df <- df %>% dplyr::select(unlist(x$covariates$values), "VALUE", "EFFECT")
      }

      if (x$is.strat.color) {
        if(length(cats) > 1) {
          dx.cats <- dx.cats[, var_val = paste0(variable, value)]
          boxplot_layers <- geom_boxplot(aes_string(x = "value", y = "VALUE", fill = "var_val"))
        }
        else boxplot_layers <- geom_boxplot(aes_string(x = "value", y = "VALUE", fill = "value"))
      }
      else {
        boxplot_layers <- geom_boxplot(aes_string(x = "value", y = "VALUE"))
      }

      x$jitter <- check_jitter(x$jitter)
      if (x$scale == "sym") scale_y <- get_scale_y(df)

      p <- ggplot(dx.cats, measure.vars = cats) +
        boxplot_layers +
        geom_boxplot(aes_string(x = "value", y = "VALUE")) +
        {if(x$is.hline)geom_hline(aes(yintercept=x$hline))} +
        facet_grid_scale(stats::as.formula("EFFECT~variable"),
                         scales = x$scale, scale_y = scale_y) +
        {if(x$is.jitter)geom_point(aes(group=VALUE),
                                   color = x$jitter$colour,
                                   size = x$jitter$size,
                                   shape=x$jitter$shape,
                                   alpha=x$jitter$alpha
                                  )
        }

      if (x$is.shrink) {
        shrink_val <- get_shrink_val(x$shrink.dx, x$shrink)
        p <- p + do.call("geom_text", shrink_val)
      }
      p
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
      x$facets$facets <- stats::as.formula("EFFECT~variable")
      p <- ggplot(dx.conts, aes_string(x = "value", y = "VALUE")) +
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

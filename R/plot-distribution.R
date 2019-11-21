
#' creates a graphic distribution object
#'
#' @param labels list of texts/titles used within the plot
#' @param jitter list set jitter parameter
#' @param facets list set the facet setting in case of histogram plot
#' @param type box for boxplot or histogram
#' @param dname name of dataset to be used
#' @param is.shrink \code{logical} if TRUE add shrinkage layer
#' @param shrink \code{list} list of parameters to tune the shrinkage
#' @param is.jitter \code{logical} if TRUE add jitter operator for points
#' @param histogram \code{list} histogram graphical parameters
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#'
#' @return distrib object
#' @family plot_pmx
#' @details
#'
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default "EBE distribution"}
#' \item {\strong{subtitle:}} {plot subtitle default empty}
#' \item {\strong{x:}} {x axis label default to "Etas"}
#' \item {\strong{y:}} {y axis label default to empty}
#' \item {\strong{legend:}} {legend title default to "random Effect"}
#' }
#' \strong{shrink} is a list that contains:
#' \itemize{
#' \item {\strong{fun:}} {shrinkage function can be \code{sd} or \code{var}}
#' \item {\strong{size:}} {shrinkage text size}
#' \item {\strong{color:}} {shrinkage text color}
#' \item {\strong{vjust:}} {shrinkage position vertical adjustment}
#' }
distrib <- function(
                    labels,
                    is.shrink,
                    type = c("box", "hist"),
                    is.jitter = FALSE,
                    jitter = NULL,
                    facets = NULL,
                    histogram = NULL,
                    shrink = NULL,
                    dname = NULL,
                    ...) {
  assert_that(is_logical(is.jitter))
  assert_that(is_list_or_null(jitter))
  assert_that(is_list_or_null(facets))
  type <- match.arg(type)
  assert_that(is_logical(is.shrink))
  assert_that(is_list_or_null(shrink))
  assert_that(is_string_or_null(dname))




  assert_that(is_list(labels))

  structure(list(
    ptype = "DIS",
    strat = TRUE,
    dname = dname,
    aess = list(x = "EFFECT", y = "VAR", z = "FUN"),
    type = type,
    is.jitter = is.jitter,
    jitter = jitter,
    facets = facets,
    histogram = histogram,
    is.shrink = is.shrink,
    shrink = shrink,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      is.smooth = FALSE,
      ...
    )
  ), class = c("distrib", "pmx_gpar"))
}



is.formula <- function(x) inherits(x, "formula")

#' merge facets formula with new formula
#'
#' @param x \code{formula} object
#' @param origin the origin formula default to ~lfacets
#'
#' @return \code{formula} object
#' @importFrom stats formula

wrap_formula <- function(x, origin = "lfacet") {
  str <- sprintf("~ %s", origin)
  if (is.character(x) && length(x) == 1) {
    str <- sprintf("%s ~ %s", origin, x)
  }

  if (length(x) == 3 && is.formula(x)) {
    str <- sprintf(
      "%s ~ %s + %s", origin,
      deparse(x[[2]]), deparse(x[[3]])
    )
  }

  if (length(x) == 2 && is.formula(x)) {
    str <- sprintf("%s ~ %s", origin, deparse(x[[2]]))
  }
  return(formula(str))
}

jitter_layer <- function(jitter, strat.color) {
  if (is.null(strat.color)) {
    do.call(geom_jitter, jitter)
  } else {
    jitter$position <- position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9)
    jitter$width <- NULL
    jitter$height <- NULL
    do.call(geom_point, jitter)
  }
}

distrib.hist <- function(dx, strat.facet, strat.color, x) {
  wrap.formula <- if (!is.null(strat.facet)) {
    wrap_formula(strat.facet, "EFFECT")
  } else {
    formula("~EFFECT")
  }
  with(x, {
    p <- ggplot(data = dx, aes_string(x = "VALUE"))
    if (!is.null(strat.color)) {
      histogram$fill <- NULL
      histogram$mapping <- aes_string(fill = strat.color)
    }
    p <- p + do.call(geom_histogram, histogram)
    if (is.shrink && !is.null(x[["shrink.dx"]])) {
      p <- p + shrinkage_layer(
        x[["shrink.dx"]],
        x$shrink, "hist", strat.color
      )
    }
    rr <- gsub("~","",as.character(wrap.formula))
    rr <- unique(rr[nzchar(rr)])
    p <-  p + if(length(rr)>=2) do.call("facet_grid", c(wrap.formula))
      else do.call("facet_wrap", c(wrap.formula, x$facets))
    p
  })
}

distrib.box <- function(dx, strat.color, strat.facet, x) {
  EFFECT <- VALUE <- NULL
  p <- ggplot(data = dx, aes_string(x = "EFFECT", y = "VALUE"))

  if (!is.null(strat.color)) {
    p <- ggplot(data = dx, aes_string(fill = strat.color, x = "EFFECT", y = "VALUE"))
  }

  if (x$is.jitter) p <- p + jitter_layer(x$jitter, strat.color)

  p <- p + geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.9))


  if (!is.null(strat.facet)) {
    rr <- gsub("~","",as.character(x$strat.facet))
    rr <- unique(rr[nzchar(rr)])
    p <-  p + if(length(rr)==2) do.call("facet_grid", c(strat.facet))
    else do.call("facet_wrap", c(strat.facet, x$facets))
  }

  if (x$is.shrink && !is.null(x[["shrink.dx"]])) {
    p <- p + shrinkage_layer(x[["shrink.dx"]], x$shrink, "box", strat.color)
  }

  ## if (x$is.hline) p <- p + do.call(geom_hline, x$hline)

  p
}



shrinkage_layer <- function(dx, shrink, type = "hist", strat.color) {
  ##
  SHRINK <- EFFECT <- POS <- annotation <- FUN <- NULL
  res <- if (type == "box") {
    shrink$mapping <-
      aes(
        label = sprintf("%s %s=%s%%", FUN, annotation, round(SHRINK * 100)),
        y = Inf
      )
    shrink$data <- dx
    shrink$data$annotation <- shrink$annotation
    shrink$position <- if (is.null(strat.color)) {
      position_dodge(width = 0.9)
    } else {
      position_jitterdodge(jitter.width = 0.1)
    }
    shrink$fun <- NULL
    shrink$annotation <- NULL
    do.call(geom_text, shrink)
  } else {
    shrink$data <- copy(dx)
    shrink$data$annotation <- shrink$annotation
    shrink$annotation <- NULL
    shrink$mapping <-
      aes(
        label = sprintf("%s=%s%%", annotation, round(SHRINK * 100)),
        y = Inf,
        x = -Inf
      )
    shrink$fun <- NULL
    do.call(geom_text, shrink)
  }
  res
}











plot_distribution <- function(x, dx, ...) {
  strat.facet <- x[["strat.facet"]]
  strat.color <- x[["strat.color"]]
  p <- if (x$type == "box") {
    distrib.box(dx, strat.color, strat.facet, x)
  } else {
    distrib.hist(dx, strat.facet, strat.color, x)
  }
  plot_pmx(x$gp, p)
}





#' Plot EBE distribution
#'
#' @param x distribution object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggplot2 plot
#' @export
#' @seealso \code{\link{distrib}}
#' @family plot_pmx
#' @import ggplot2
#'
plot_pmx.distrib <- function(x, dx, ...) {
  plot_distribution(x, dx, ...)
}

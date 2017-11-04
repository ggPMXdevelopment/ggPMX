
#' creates a graphic distribution object
#'
#' @param labels list of texts/titles used within the plot
#' @param jitter list set jitter parameter
#' @param facets list set the facet setting in case of histogram plot
#' @param type box for boxplot or histogram
#' @param dname name of dataset to be used
#' @param has.shrink \code{logical} if TRUE add shrinkage layer
#' @param shrink \code{list} list of parameters to tune the shrinkage
#' @param has.jitter \code{logical} if TRUE add jitter operator for points
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
#' \item {\strong{legend:}} {legend titile default to "random Effect"}
#' }
#' \strong{shrink} is a list that contains:
#' \itemize{
#' \item {\strong{fun:}} {shrinkage function can be \code{sd} or \code{var}}
#' \item {\strong{size:}} {shrinkage text size}
#' \item {\strong{color:}} {shrinkage text color}
#' \item {\strong{vjust:}} {shrinkage position vertical adjustment}
#' }
#'
#'
#' @export
distrib <- function(
  labels,
  has.jitter = FALSE,
  jitter = list(shape = 1, color = "grey50", width = 0.1),
  facets = list(scales = "free_y", nrow = 3),
  type = c("box", "hist"),
  has.shrink = FALSE,
  histogram=list(binwidth = 1 / 30, position = "dodge"),
  shrink=list(fun = "sd", size = 5, color = "black"),
  dname = NULL,
  ...) {
  assert_that(is_logical(has.jitter))
  assert_that(is_list(jitter))
  assert_that(is_list(facets))
  type <- match.arg(type)
  assert_that(is_logical(has.shrink))
  assert_that(is_list(shrink))
  assert_that(is_string_or_null(dname))
  if (is.null(dname)) dname <- "eta"
  
  
  if (missing(labels)) {
    labels <- list(
      title = "EBE distribution",
      subtitle = "",
      x = "Etas",
      y = "",
      legend = "Random effect"
    )
  }
  assert_that(is_list(labels))
  
  structure(list(
    ptype = "DIS",
    strat=TRUE,
    dname = dname,
    aess = list(x = "EFFECT", y = "VAR", z = "FUN"),
    type = type,
    has.jitter = has.jitter,
    jitter = jitter,
    facets = facets,
    histogram = histogram,
    has.shrink = has.shrink,
    shrink = shrink,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      has.smooth = FALSE,
      has.band = FALSE, ...
    )
  ), class = c("distrib", "pmx_gpar"))
}



is.formula <- function(x) inherits(x, "formula")

#' merge facets formula with new formula
#'
#' @param x \code{formula} object
#' @param origin the origin formula defualt to ~lfacets
#'
#' @return \code{formula} object
#' @importFrom stats formula

wrap_formula <- function(x, origin="lfacet") {
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
      histogram$mapping <- aes_string(fill = strat.color)
    }
    p <- p + do.call(geom_histogram, histogram)
    facets$facets <- wrap.formula
    p <- p + do.call(facet_wrap, facets)
    if (has.shrink) p <- p + shrinkage_layer(x[["shrink.dx"]], x$shrink, "hist", strat.color)
    
    p
  })
}

distrib.box <- function(dx, strat.color, strat.facet, x) {
  EFFECT <- VALUE <- NULL
  p <- ggplot(data = dx, aes_string(x = "EFFECT", y = "VALUE"))
  
  if (!is.null(strat.color)) {
    p <- ggplot(data = dx, aes_string(fill = strat.color, x = "EFFECT", y = "VALUE"))
  }
  
  if (x$has.jitter) p <- p + jitter_layer(x$jitter, strat.color)
  
  p <- p + geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.9))
  
  
  if (!is.null(strat.facet)) {
    p <- p + with(x$facets, facet_wrap(
      strat.facet, scales = scales,
      nrow = nrow
    ))
  }
  
  if (x$has.shrink) p <- p + shrinkage_layer(x[["shrink.dx"]], x$shrink, "box", strat.color)
  
  p
}



shrinkage_layer <- function(dx, shrink, type="hist", strat.color) {
  ##
  SHRINK <- EFFECT <- POS <- NULL
  res <- if (type == "box") {
    shrink$mapping <-
      aes(
        label = sprintf("%s%%", round(SHRINK * 100)),
        y = Inf
      )
    shrink$data <- dx
    shrink$position <- if (is.null(strat.color)) position_dodge(width = 0.9)
    else position_jitterdodge(jitter.width = 0.1)
    shrink$fun <- NULL
    do.call(geom_text, shrink)
  } else {
    shrink$label <- sprintf("shrinkage=%s%%", round(dx$SHRINK * 100))
    shrink$geom <- "text"
    shrink$x <- -Inf
    shrink$y <- Inf
    shrink$fun <- NULL
    do.call(annotate, shrink)
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




pmx_qq_stats = function(points){
  sample <- sort(points)
  distribution = stats::qnorm
  n <- length(sample)
  line.p = c(.25, .75)
  quantiles <- stats::ppoints(n)

  theoretical <- do.call(
    distribution,
    c(p = quote(quantiles))
  )
  
  x_coords <- do.call(distribution, c(list(p = line.p)))
  y_coords <- quantile(sample, line.p)
  slope <- diff(y_coords) / diff(x_coords)
  intercept <- y_coords[1L] - slope * x_coords[1L]
  x <- range(theoretical)
  
  data.table(x = x, y = slope * x + intercept)
}




#' This function creates a qq plot object
#'
#' @param x \code{character} variable name to sample
#' @param labels list of texts/titles used within the plot
#' @param dname name of dataset to be used
#' @param point \code{list} geom_point attributes color, shape,...
#' @param reference_line \code{list} geom_line attributes. Used only for pmx_plot_eta_qq
#' @param facets \code{list}
#' @param xmax \code{logical} if FALSE do not use max(aes(x)) as limits default to TRUE
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return \code{pmx_qq} object
#' @family qq_plot
#' @details
#'
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default  "EBE vs. covariates"}
#' \item {\strong{x:}} {x axis label default to "Etas"}
#' \item {\strong{y:}} {y axis label default to empty}
#' }
#'
#' #' \strong{point} is a list that contains:
#' \itemize{
#' \item {\strong{shape:}} {default to 1}
#' \item {\strong{color:}} {default to black}
#' \item {\strong{size:}} {default to 1}
#' }
#'
#' @export
pmx_qq <- function(
                   x,
                   labels,
                   dname = NULL,
                   point=NULL,
                   xmax=TRUE,
                   facets=NULL,
                   reference_line=NULL,
                   ...) {
  assert_that(is_string_or_null(dname))
  if (is.null(dname)) dname <- "predictions"


  if (missing(labels)) {
    labels <- list(
      title = sprintf("QQ plot: %s", x),
      y = "",
      x = "",
      subtitle = ""
    )
  }
  assert_that(is_list(labels))
  default_point <- list(shape = 1, colour = "black", size = 1)
  point <- l_left_join(default_point, point)
  assert_that(is_list_or_null(facets))
  assert_that(is_list_or_null(point))
  assert_that(is_list_or_null(reference_line))
  
  labels$subtitle <- ""
  structure(list(
    ptype = "PMX_QQ",
    strat = TRUE,
    x = x,
    dname = dname,
    point = point,
    reference_line=reference_line,
    xmax = xmax,
    facets = facets,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      is.smooth = FALSE
    )
  ), class = c("pmx_qq", "pmx_gpar"))
}




















#' This function plot EBE versus covariates using qq plots
#'
#' @param x pmx_qq object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggplot2 plot
#' @export
#' @seealso \code{\link{eta_cov}}
#' @family plot_pmx
#' @import ggplot2
#'
plot_pmx.pmx_qq <- function(x, dx, ...) {
  if (!(x$x %in% names(dx))) return(NULL)
  dx <- dx[!is.infinite(get(x$x))]
  if (!is.null(x$omega) && "EFFECT" %in% names(dx)) {
    dx <- merge(dx, x$omega, by = "EFFECT")
  }
  p <- ggplot(dx, aes_string(sample = x$x)) +
    with(
      x$point,
      geom_point(
        stat = "qq", shape = shape, colour = colour,
        size = size
      )
    )
  

  strat.facet <- x[["strat.facet"]]

  if ("EFFECT" %in% names(dx)) {
    grp <- as.character(strat.facet)
    grp <- unique(c("EFFECT",grep("~",grp,value = T,invert = T)))
    dx[,c("x","y"):=pmx_qq_stats(get(x$x)),grp]
    if (!is.null(x$reference_line)){
      x$reference_line$mapping <- aes_string(x="x",y="y")
      p <- p + do.call(geom_line,x$reference_line)
    }
    wrap.formula <- if (!is.null(strat.facet)) {
      wrap_formula(strat.facet, "EFFECT")
    } else {
      formula("~EFFECT")
    }
    p <- p + do.call("facet_wrap", c(wrap.formula, x$facets))
  } else {
    if (!is.null(strat.facet)) {
      if (is.character(strat.facet)) {
        wrap.formula <- formula(paste0("~", strat.facet))
        p <- p + do.call("facet_wrap", c(wrap.formula, x$facets))
      }
    }
  }

  strat.color <- x[["strat.color"]]
  if (!is.null(strat.color)) {
    p <- p %+% geom_point(stat = "qq", aes_string(colour = strat.color))
  }


  if (!is.null(p)) p <- plot_pmx(x$gp, p)

  if (x$xmax) {
    xmin <- min(dx[, x$x, with = FALSE], na.rm = TRUE)
    xmax <- max(dx[, x$x, with = FALSE], na.rm = TRUE)
    xrange <- c(xmin - .001 * (xmax - xmin), xmax + .001 * (xmax - xmin))
    p <- p +
      coord_cartesian(xlim = xrange, ylim = xrange) +
      theme(aspect.ratio = 1)
  }
  p
}

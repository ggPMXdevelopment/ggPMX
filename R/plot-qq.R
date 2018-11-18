


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
  data.table(intercept=intercept,slope=slope)
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
  
  strat.facet <- x[["strat.facet"]]
  strat.color <- x[["strat.color"]]
  vec.color <- c(grep("~",strat.color,invert=TRUE,value=TRUE))
  vec.facet <- c(grep("~",strat.facet,invert=TRUE,value=TRUE))
  
  grp <- c(vec.color,vec.facet)
  if(sum(nchar(grp))==0) grp <- NULL
  dx.ref <- if ("EFFECT" %in% names(dx)) {
    dx[,pmx_qq_stats(get(x$x)),c("EFFECT",grp)]
  }else{
    dx[,pmx_qq_stats(get(x$x)),grp]
  }
  
  
  p <- ggplot(dx, aes_string(sample = x$x)) +
    with(
      x$point,
      geom_point(
        stat = "qq", shape = shape, colour = colour,
        size = size
      )
    )
  


  if (!is.null(x$reference_line)){
    x$reference_line$mapping <- aes_string(slope="slope",intercept = "intercept")
    if (is.null(strat.color)){
      x$reference_line$colour <- NULL
      x$reference_line$mapping <- aes_string(slope="slope",intercept = "intercept",
                                             colour=strat.color)
    }
    x$reference_line$data <- dx.ref
    p <- p + do.call("geom_abline",x$reference_line)
  }
  
  layer_facet <- if ("EFFECT" %in% names(dx)) {
     if (!is.null(strat.facet)){
      wf <- wrap_formula(strat.facet, "EFFECT")
       x$facets$nrow <- NULL
       x$facets$ncol <- NULL
       do.call("facet_grid", c(wf, x$facets))
     }
    else {
      wf <- formula("~EFFECT")
      do.call("facet_wrap", c(wf, x$facets))
    }
  } else {
    if (!is.null(strat.facet)) {
      if (is.character(strat.facet)) {
        wf <- formula(paste0("~", strat.facet))
        do.call("facet_wrap", c(wf, x$facets))
      }
    }
  }
  if(!is.null(layer_facet))
    p <- p + layer_facet
  

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

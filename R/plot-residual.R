

default_residual <- function(...) {
  list(
    point = list(shape = 1, color = "black", size = 1),
    hline = list(yintercept = 0)
  )
}


#' Create a residual object
#'
#' @param x x axis aesthetics
#' @param y y axis aesthetics
#' @param labels list that contain title,subtitle, axis labels
#' @param point geom point graphical parameters
#' @param add_hline logical if TRUE add horizontal line y=0 ( TRUE by default)
#' @param hline geom hline graphical parameters
#' @param dname name of dataset to be used
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return a residual object
#' @export
#' @family plot_pmx
#' @seealso \code{\link{plot_pmx.residual}}
#' @details
#' Some parameters are a list of parameters :
#'
#' \strong{point} is a list that contains:
#' \itemize{
#' \item {\strong{shape:}} {default to 1}
#' \item {\strong{color:}} {default to black}
#' \item {\strong{size:}} {default to 1}
#' }
#'
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default to AES_X versue AES_Y}
#' \item {\strong{subtitle:}} {plot subtitle default empty}
#' \item {\strong{x:}} {x axis label default to AES_X}
#' \item {\strong{y:}} {y axis label default to AES_Y}
#' }
residual <- function(x, y, labels = NULL, point = NULL, add_hline=TRUE, hline=NULL, dname=NULL, ...) {
  ## default labels parameters
  ## TODO pout all defaultas option
  stopifnot(!missing(x))
  stopifnot(!missing(y))
  aess <- list(x = x, y = y)
  default_labels <- list(
    title = paste(rev(aess), collapse = " versus "),
    subtitle = "",
    x = aess[["x"]],
    y = aess[["y"]]
  )
  assert_that(is_list_or_null(labels))
  assert_that(is_list_or_null(point))
  assert_that(is_list_or_null(hline))
  assert_that(is_string_or_null(dname))

  labels <- l_left_join(default_labels, labels)
  default_point <- list(shape = 1, color = "black", size = 1)
  default_hline <- list(yintercept = 0)
  point <- l_left_join(default_point, point)
  hline <- l_left_join(default_hline, hline)
  if (is.null(dname)) dname <- "predictions"

  structure(
    list(
      ptype = "RES",
      dname = dname,
      aess = aess,
      point = point,
      add_hline = add_hline,
      hline = hline,
      gp = pmx_gpar(labels = labels, ...)
    ), class = c("residual", "pmx_gpar")
  )
}


extend_range <-
  function(x, r = range(x, na.rm = TRUE), f = 0.05) {
    if (!missing(r) && length(r) != 2) {
      stop("'r' must be a \"range\", hence of length 2")
    }
    rr <- r + c(-f, f) * diff(r)
    if (rr[1] <= 0) rr[1] <- 0.01
    rr
  }


#' Plot residual object
#'
#' @param x residual object
#' @param dx data set
#' @param ... not used for the moment

#'
#' @return ggplot2 object
#' @seealso \code{\link{residual}}
#' @family plot_pmx
#' @export
plot_pmx.residual <- function(x, dx, ...) {
  with(x, {
    dx <- dx[!is.infinite(get(aess$x)) & !is.infinite(get(aess$y))]


    p <- ggplot(dx, with(aess, ggplot2::aes_string(x, y)))

    p <- p + do.call(geom_point, point)
    if (add_hline) p <- p + do.call(geom_hline, hline)
    p <- plot_pmx(gp, p)

    strat.color <- x[["strat.color"]]
    strat.facet <- x[["strat.facet"]]
    if (!is.null(strat.color)) {
      p <- p %+% geom_point(aes_string(color = strat.color))
    }

    if (!is.null(strat.facet)) {
      if (is.character(strat.facet)) {
        strat.facet <- formula(paste0("~", strat.facet))
      }
      p <- p + facet_grid(strat.facet)
    }
    if (aess$y == "DV") {
      xrange <- extend_range(dx[, c(aess$x, aess$y), with = FALSE])
      p <- p +
        coord_cartesian(xlim = xrange, ylim = xrange) +
        theme(aspect.ratio = 1)
    }
    p
  })
}

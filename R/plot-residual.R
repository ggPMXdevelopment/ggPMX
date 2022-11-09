

#' This function create a residual for each observed value and also generates a residual
#' distribution
#'
#' @param x x axis aesthetics
#' @param y y axis aesthetics
#' @param labels list that contain title,subtitle, axis labels
#' @param point geom point graphical parameters
#' @param is.hline logical if TRUE add horizontal line y=0 ( TRUE by default)
#' @param hline geom hline graphical parameters
#' @param dname name of dataset to be used
#' @param facets \code{list} wrap facetting in case of strat.facet
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#' @param bloq \code{pmxBLOQ} object created by \code{\link{pmx_bloq}}

#'
#' @return a residual object
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
#' \item {\strong{title:}} {plot title default to AES_X versus AES_Y}
#' \item {\strong{subtitle:}} {plot subtitle default empty}
#' \item {\strong{x:}} {x axis label default to AES_X}
#' \item {\strong{y:}} {y axis label default to AES_Y}
#' }
residual <- function(x, y, labels = NULL, point = NULL, is.hline = FALSE,
                     hline = NULL, dname = NULL, facets = NULL, bloq = NULL, ...) {

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
  default_point <- list(shape = 1, colour = "black", size = 1)
  default_hline <- list(yintercept = 0)
  point <- l_left_join(default_point, point)
  hline <- l_left_join(default_hline, hline)
  if (is.null(dname)) dname <- "predictions"

  structure(
    list(
      ptype = "SCATTER",
      strat = TRUE,
      dname = dname,
      aess = aess,
      point = point,
      is.hline = is.hline,
      hline = hline,
      facets = facets,
      bloq = bloq,
      gp = pmx_gpar(labels = labels, ...)
    ),
    class = c("residual", "pmx_gpar")
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


#' This function plots residual for each observed value by finding the difference between observed and predicted points. It also fits a distribution to the residual value.
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
    if (!all(c(aess$x, aess$y) %in% names(dx))) {
      return(NULL)
    }
    dx <- dx[!is.infinite(get(aess$x)) & !is.infinite(get(aess$y))]

    p <- ggplot(dx, with(aess, ggplot2::aes_string(x, y)))

    p <- p + do.call(geom_point, point)

    if (!is.null(bloq)) {
      bloq$data <- dx[get(bloq$cens) != 0]
      bloq$cens <- bloq$limit <- NULL
      p <- p + do.call(geom_point, bloq)
    }

    if (is.hline) p <- p + do.call(geom_hline, hline)


    if (aess$y == "DV" && !(gp$scale_x_log10 || gp$scale_y_log10)) {
      xrange <- extend_range(dx[, c(aess$x, aess$y), with = FALSE])
      if (!is.null(gp$ranges)) {
        if (is.null(gp$ranges$x)) {
          rng <- gp$ranges$y
        } else {
          if (is.null(gp$ranges$y)) {
            rng <- gp$ranges$x
          } else {
            rng <- c(
              max(gp$ranges$x[1], gp$ranges$y[1]),
              min(gp$ranges$x[2], gp$ranges$y[2])
            )
          }
        }
        xrange[1] <- max(xrange[1], rng[1])
        xrange[2] <- min(xrange[2], rng[2])
        gp$ranges$x <- xrange
        gp$ranges$y <- xrange
      }
      p <- p +
        coord_cartesian(xlim = xrange, ylim = xrange) +
        theme(aspect.ratio = 1)
    }

    if (is.null(gp$ranges) || is.null(gp$ranges$y)) {
      if (aess$y %in% c("NPDE", "IWRES") && !gp$scale_y_log10 && is.null(x$trans)) {
        mm <- max(abs(dx[, aess$y, with = FALSE]), na.rm = TRUE)
        if (is.null(gp$ranges)) {
          gp$ranges <- list(y = c(-mm, mm))
        } else  {
          gp$ranges$y <- c(-mm, mm)
        }
      }
    }

    strat.color <- x[["strat.color"]]
    strat.facet <- x[["strat.facet"]]
    if (!is.null(strat.color)) {
      p <- p %+% geom_point(aes_string(colour = strat.color))
    }

    if (!is.null(strat.facet)) {
      if (is.character(strat.facet)) {
        strat.facet <- formula(paste0("~", strat.facet))
      }
      p <- p + do.call("facet_wrap", c(strat.facet, facets))
    }


    p <- plot_pmx(gp, p)


    p
  })
}

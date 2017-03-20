

#' Create a residual object
#'
#' @param x x axis aesthetics
#' @param y y axis aesthetics
#' @param labels list that contain title,subtitle, axis labels
#' @param point geom point graphical parameters
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return a residual object
#' @export
#' @seealso \code{\link{plot_pmx.residual}}

residual <- function(x, y, labels = NULL, point = NULL, ...){
  ## default labels parameters
  ## TODO pout all defaultas option
  stopifnot(!missing(x))
  stopifnot(!missing(y))
  aess <- list(x = x, y = y)
  default_labels <- list(
    title=paste(aess, collapse = " versus "),
    subtitle = "",
    x = abbrev(aess[["x"]]),
    y = abbrev(aess[["y"]])
  )
  labels <- l_left_join(default_labels, labels)
  default_point <- list(shape = 1, color = "black", size = 1)
  point <- l_left_join(default_point, point)

  structure(
    list(
      aess = aess,
      point = point,
      gp = pmx_gpar(labels = labels, ...)
    ), class=c("residual", "pmx_gpar"))
}



#' Plot residual object
#'
#' @param x residual object
#' @param dx data set

#'
#' @return ggplot2 object
#' @seealso \code{\link{residual}}
#' @family plot_pmx
#' @export
plot_pmx.residual <- function(x, dx){
  stopifnot(is.pmx_gpar(x))
  with(x,
       {
         p <-
           ggplot2::ggplot(dx, with(aess, ggplot2::aes_string(x, y)))+
           with(point, geom_point(shape = shape, color = color))
         if("z" %in% names(gp))
         p <- p + ggplot2::facet_wrap(as.formula(paste('~' , gp$z)))
         p <- plot_pmx(gp, p)
         p
       })
}




#' Create a residual object
#'
#' @param x x axis aesthetics
#' @param y y axis aesthetics
#' @param labels list that contain title,subtitle, axis labels
#' @param point geom point graphical parameters
#' @param add_hline logical if TRUE add horizontal line y=0 ( TRUE by default)
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return a residual object
#' @export
#' @family plot_pmx
#' @seealso \code{\link{plot_pmx.residual}}

residual <- function(x, y, labels = NULL, point = NULL,add_hline=TRUE, ...){
  ## default labels parameters
  ## TODO pout all defaultas option
  stopifnot(!missing(x))
  stopifnot(!missing(y))
  aess <- list(x = x, y = y)
  default_labels <- list(
    title=paste(rev(aess), collapse = " versus "),
    subtitle = "",
    x = aess[["x"]],
    y = aess[["y"]]
  )
  assert_that(is_list_or_null(labels))
  
  labels <- l_left_join(default_labels, labels)
  default_point <- list(shape = 1, color = "black", size = 1)
  point <- l_left_join(default_point, point)
  
  structure(
    list(
      aess = aess,
      point = point,
      add_hline=add_hline,
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
  assert_that(is_pmx_gpar(x))
  with(x,
       {
         if(!is.null(x[["filter"]])){
           dx <- x[["filter"]](dx)
         }
         p <-
           ggplot2::ggplot(dx, with(aess, ggplot2::aes_string(x, y)))+
           with(point, geom_point(shape = shape, color = color))+
           if(add_hline)geom_hline(yintercept = 0)
         p <- plot_pmx(gp, p)
         p
       })
}



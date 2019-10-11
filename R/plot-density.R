



#' Creates a density plot object
#'
#' @param x \code{character} variable name to sample
#' @param labels list of texts/titles used within the plot
#' @param dname name of dataset to be used
#' @param xlim \code{numeric} x axis limits
#' @param var_line \code{list} variable density graphics parameters
#' @param snd_line \code{list} normal density graphics parameters
#' @param vline \code{list} vertical line graphics parameters
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @details
#'
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default  "IWRES density plot"}
#' \item {\strong{x:}} {x axis label default to "Etas"}
#' \item {\strong{y:}} {y axis label default to empty}
#' }
#'
#' \strong{var_line} is a list that contains:
#' \itemize{
#' \item {\strong{linetype:}} {default to 1}
#' \item {\strong{color:}} {default to black}
#' \item {\strong{size:}} {default to 1}
#' }
#'
#' \strong{snd_line} is a list that contains:
#' \itemize{
#' \item {\strong{linetype:}} {default to 2}
#' \item {\strong{color:}} {default to black}
#' \item {\strong{size:}} {default to 1}
#' }
#'
#' \strong{vline} is a list that contains:
#' \itemize{
#' \item {\strong{linetype:}} {default to 3}
#' \item {\strong{color:}} {default to black}
#' \item {\strong{size:}} {default to 1}
#' }
#'
pmx_dens <- function(
                     x,
                     labels,
                     dname = NULL,
                     xlim = 3,
                     var_line = NULL,
                     snd_line = NULL,
                     vline = NULL,
                     ...) {
  assert_that(is_string_or_null(dname))
  if (is.null(dname)) dname <- "predictions"


  if (missing(labels)) {
    labels <- list(
      title = sprintf("Density plot of %s", x),
      y = "",
      x = "",
      subtitle = ""
    )
  }
  assert_that(is_list(labels))
  default_var_line <- list(linetype = 1, colour = "black", size = 1)
  var_line <- l_left_join(default_var_line, var_line)
  default_snd_line <- list(linetype = 2, colour = "black", size = 1)
  snd_line <- l_left_join(default_snd_line, snd_line)
  default_vline <- list(linetype = 3, colour = "black", size = 1)
  vline <- l_left_join(default_vline, snd_line)
  labels$subtitle <- ""
  structure(list(
    ptype = "PMX_DENS",
    strat = TRUE,
    x = x,
    dname = dname,
    xlim = xlim,
    var_line = var_line,
    snd_line = snd_line,
    vline = vline,
    gp = pmx_gpar(
      labels = labels,
      discrete = TRUE,
      is.smooth = FALSE
    )
  ), class = c("pmx_dens", "pmx_gpar"))
}




















#' This function plot EBE versus covariates using qq plots
#'
#' @param x eta_cov object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggplot2 plot
#' @export
#' @seealso \code{\link{eta_cov}}
#' @family plot_pmx
#' @import ggplot2
#'
plot_pmx.pmx_dens <- function(x, dx, ...) {
  dx <- dx[!is.infinite(get(x$x))]


  with(x, {
    xrange <- c(-xlim, xlim)
    dens_layer <- if (!is.null(var_line)) {
      params <- var_line
      do.call(geom_density, params)
    }

    snd_layer <- if (!is.null(snd_line)) {
      params <- append(
        list(
          data = data.frame(x = xrange), fun = dnorm, mapping = aes(x)
        ),
        snd_line
      )
      do.call(stat_function, params)
    }

    vline_layer <- if (!is.null(vline)) {
      params <- append(list(xintercept = 0), vline)

      do.call(geom_vline, params)
    }


    p <- ggplot(dx, aes(x = get(x))) +
      dens_layer + snd_layer + vline_layer



    if (!is.null(p)) p <- plot_pmx(gp, p)

    p <- p +
      coord_cartesian(xlim = xrange) +
      theme(aspect.ratio = 1)
    p
  })
}

#' Define ggPMX theme
#'
#' This theme is a simple wrapper gdoc theme
#' from ggthems package.
#' @param ... can contain any valid argument of \code{ggplot2} \code{\link[ggplot2]{theme}}
#' object.

#' @import ggplot2
#' @export
pmx_theme <- function(...) {
  theme_bw() +
    theme(
      strip.text = element_text(size = 14),
      strip.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.position = "none"
    ) %+replace%
    theme(...)
}

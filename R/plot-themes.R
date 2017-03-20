#' Define ggPMX theme
#'
#' This theme is a simple wrapper gdoc theme
#' from ggthems package.
#' @param ... can contain any valid argument of \code{ggplot2} \code{\link[ggplot2]{theme}}
#' object.

#' @importFrom ggplot2 theme %+replace% element_text
#' @importFrom ggthemes theme_gdocs
#' @export
pmx_theme <- function(...) {
  theme_gdocs() +
    theme(plot.subtitle=element_text( face="italic"))  %+replace%
    theme(...)
}

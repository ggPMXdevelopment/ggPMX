#' This is a generic plot method that produces all plots by default described in pmx model
#' evaluation guidance.
#'
#' @param x  object to plot
#' @param dx data.table , plot source data
#' @param ... extra argument (not used)
#' @seealso \code{\link{pmx_gpar}}.
#' @family plot_pmx
#' @export
plot_pmx <- function(x, dx, ...) UseMethod("plot_pmx")

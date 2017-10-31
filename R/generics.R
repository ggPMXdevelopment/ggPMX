#' Generic plot method for pmx objects
#'
#' @param x  object to plot
#' @param dx data.table , plot source data
#' @param ... extra argument (not used)
#' @seealso \code{\link{pmx_gpar}}.
#' @family plot_pmx
#' @export
plot_pmx <- function(x, dx, ...) UseMethod("plot_pmx")


#' Handling pmx Graphical parameters
#' @param is.title \code{logical} if TRUE then a title is used for the plot
#' @param labels list of labels, like title, subtitle, x , y
#' @param axis.title list or element_text (same as ggplot2 axis.title theme)
#' @param which_pages page(s) to display; if "all" display all pages,
#' if 1 display first page, if c(1,2) display first and second pages
#' @param print if TRUE the ouptut will be a print not a ggplot2. This
#' is useful for rmarkdwon output to avoid verbose list index print.
#' @param axis.text list or element_text (same as ggplot2 axis.text theme)
#' @param ranges limits of x/y ranges
#' @param is.smooth logical if set to TRUE add smooth layer
#' @param smooth smooth layer parameters
#' @param is.band logical if TRUE add horizontal band
#' @param band horizontal band parameters
#' @param is.draft logical if TRUE add draft layer
#' @param draft draft layer parameters
#' @param is.identity_line \code{logical} if TRUE add y=x line
#' @param smooth_with_bloq \code{logical} if TRUE perform spline in plots with BLOQ data
#' @param identity_line \code{list} y=x aes properties
#' @param discrete logical if TRUE x axis is discrete(FALSE by default)
#' @param scale_x_log10 logical if TRUE add scale_x_log10 layer
#' @param scale_y_log10 logical if TRUE add scale_y_log10 layer
#' @param color.scales \code{list} define scales parameter in case of strat.color \code{\link{pmx_settings}}
#' @param is.legend \code{logical} if TRUE x axis is discrete(FALSE by default)
#' @param legend.position \code{charcater} legend position it takes the same value as the equivalent ggplot2 parameter
#'
#' @details

#' This object contains all general graphic settings. It used internally by all
#' pmx_plot(generic function) to set the default behavior.


#' @return  An object of class \code{"pmx_gpar"}.
#' @export

pmx_gpar <-
  function(
             is.title,
             labels,
             axis.title,
             which_pages,
             print,
             axis.text,
             ranges,
             is.smooth,
             smooth,
             is.band,
             band,
             is.draft,
             draft,
             discrete,
             is.identity_line,
             identity_line,
             smooth_with_bloq,
             scale_x_log10,
             scale_y_log10,
             color.scales,
             is.legend,
             legend.position) {

    ## join with default values
    default_yaml <-
      file.path(system.file(package = "ggPMX"), "init", "gpar.yaml")
    default_gpars <- yaml.load_file(default_yaml)
    gpars <- as.list(match.call(expand.dots = TRUE)[-1])
    gp <- default_gpars
    if (length(gpars) > 0) {
      gpars <- mget(names(gpars))
      gp <- l_left_join(default_gpars, gpars)
    }

    class(gp) <- c("pmx_gpar", "list")
    gp
  }

#' Check if an object is a pmx_gpar class
#'
#' @param x pmx_gpar object
#'
#' @return \code{logical} returns TRUE if it is a \code{pmx_gpar} object
#' @export

is.pmx_gpar <- function(x) {
  inherits(x, "pmx_gpar")
}

#' Print pmx_gpar object
#'
#' @param x pmx_gpar object
#' @param ... argument passed to print ( to satisfy generic)
#'
#' @return a character description of graphical parameters
#' @export

print.pmx_gpar <- function(x, ...) {
  assert_that(is_pmx_gpar(x))
  print(unclass(x), ...)
  invisible(x)
}

rep_or_null <- function(x, length.out) {
  if (is.null(x)) return(x) 
  rep(x, length.out = length.out)
}

#' Method for subsetting "pmx_gpar" objects
#'
#' @param x pmx_gpar object
#' @param index can be character/integer of element
#' @param ... other parameter (not used just for generic)
#'
#' @return if exists the parameter description
#' @export
`[.pmx_gpar` <- function(x, index, ...) {
  if (length(x) == 0) {
    return(pmx_gpar())
  }
  maxn <- do.call("max", lapply(x, length))
  newgp <- lapply(x, function(val) {
    if (is.null(val)) {
      return(NULL)
    }

    rep(val, length.out = maxn)[index]
  })

  class(newgp) <- "pmx_gpar"
  newgp
}


#' Handling pmx Graphical parameters
#' @param labels list of labels, like title, subtitle, xlab , ylab
#' @param  axis.title list of axis title parameter : font size
#' @param axis.text list of axis title parameter : font size
#' @param ranges limits of x/y ranges
#' @param has.smooth logical if set to TRUE add smooth layer
#' @param smooth smooth layer parameters
#' @param has.band logical if TRUE add horizontal band
#' @param band horizontal band parameters
#' @param is.draft logical if TRUE add draft layer
#' @param draft draft layer parameters
#' @param discrete logical if TRUE x axis is discrete(FALSE by default)
#' @param ... extra arguments (not used yet)
#'
#'
#' @details

#' This object contains all general graphic settings. It used internally by all
#' pmx_plot(generic fucntion) to set the default behavior.


#' @return  An object of class \code{"pmx_gpar"}.
#' @export

pmx_gpar <-
  function(
    labels,
    dname,
    axis.title = c(size = 12),
    axis.text = c(size = 14),
    ranges = NULL,
    has.smooth = FALSE,
    smooth = list(se = FALSE, linetype = 2, size = 1.5, method = 'loess'),
    has.band = FALSE,
    band = list(y = c(-2, 2), linetype = 2, size = 0.5),
    is.draft = TRUE,
    draft = list(size = 20, label = "DRAFT", color = 'grey50'),
    discrete=FALSE,
    ...) {
    gp <- .valid_pmx_gpar(list(
      labels = labels,
      dname = dname,
      axis.title = axis.title,
      axis.text = axis.text,
      ranges = ranges,
      has.smooth = has.smooth,
      smooth = smooth,
      has.band = has.band,
      band = band,
      is.draft = is.draft,
      draft = draft,
      discrete = discrete,
      ...))
    class(gp) <- "pmx_gpar"
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
#' @param ... argument passed to print ( to statisfy generic)
#'
#' @return a character description of graphical parameters
#' @export

print.pmx_gpar <- function(x, ...) {
  assert_that(is_pmx_gpar(x))
  print(unclass(x), ...)
  invisible(x)
}

.valid_pmx_gpar <- function(gpars) {
  ## TDOD add assertions about
  ## graphical parametrs

  ## join with default values
  default_yaml <- 
    file.path(system.file(package="ggPMX"), "init", "gpar.yaml")
  default_gpars <- yaml.load_file(default_yaml)
  l_left_join(default_gpars, gpars)
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
  if (length(x) == 0)
    return(pmx_gpar())
  maxn <- do.call("max", lapply(x, length))
  newgp <- lapply(x, rep, length.out = maxn)
  newgp <- lapply(X = newgp, FUN = "[", index, ...)
  class(newgp) <- "pmx_gpar"
  newgp
}


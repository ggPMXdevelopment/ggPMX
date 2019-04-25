# This allows user to define package globals , options
.globals <- new.env(parent = emptyenv())

.globals$options <- list()

# Aesthetic aliases
.globals$ggplot <- c(
  "col" = "colour",
  "color" = "colour",
  "pch" = "shape",
  "cex" = "size",
  "lty" = "linetype",
  "lwd" = "size",
  "srt" = "angle",
  "adj" = "hjust",
  "bg" = "fill",
  "fg" = "colour",
  "min" = "ymin",
  "max" = "ymax"
)

#' Get ggPMX Option
#'
#' @param name Name of an option to get.
#' @param default Value to be returned if the option is not currently set.
#'
#' @examples
#' \dontrun{
#' pmxOptions(myOption = 10)
#' getPmxOption("myOption")
#' }
#' @export
getPmxOption <- function(name, default = NULL) {
  assert_that(is_string(name))

  if (name %in% names(.globals$options)) {
    .globals$options[[name]]
  } else {
    default
  }
}



#' This function can be used to set ggPMX options
#'
#' \code{getPmxOption} retrieves the value of a ggPMX option.
#' \code{ggPMXOptions} sets the value of ggPMX options; it can also be used to
#' return a list of all currently-set ggPMX options.
#'
#'
#' @param ... Options to set, with the form \code{name = value}.
#'
#' @details
#' There is a global option set, which is available by default.
#' @section Options used in ggPMX:
#'
#' \itemize{
#' \item {\strong{template_dir:}} {path to template directory}
#' }
#'
#' @examples
#' \dontrun{
#' pmxOptions(template_dir=PATH_TO_CUSTOM_CONFIGURATION)
#' }
#' @export
pmxOptions <- function(...) {
  newOpts <- list(...)
  if (length(newOpts) > 0) {
    if ("template_dir" %in% names(newOpts)) {
      .globals$options <- newOpts
    } else {
      .globals$options <- .globals$options["template_dir"]
    }

    .globals$options <- dropNulls.(mergeVectors.(.globals$options, newOpts))
    invisible(.globals$options)
  } else {
    .globals$options
  }
}


checkPmxOption <- function(value, pmxname, default=NULL) {
  call <- match.call()
  if (missing(value)) {
    value <- getPmxOption(pmxname, default)
  }
  if (is.null(value)) {
    stop(
      sprintf(
        "Please set a %s argument or set global %s option",
        deparse(call$value), pmxname
      )
    )
  }
  value
}

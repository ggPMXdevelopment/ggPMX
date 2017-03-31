# A scope where we can put package globals , options
.globals <- new.env(parent = emptyenv())

.globals$options <- list()

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
  
  if (name %in% names(.globals$options))
    .globals$options[[name]]
  else
    default
}



#' Get or set ggPMX options
#'
#' \code{getPmxOption} retrieves the value of a Shiny option.
#' \code{ggPMXOptions} sets the value of ggPMX options; it can also be used to
#' return a list of all currently-set ggPMX options.
#'
#' There is a global option set, which is available by default.
#'
#' @param ... Options to set, with the form \code{name = value}.
#'
#' @examples
#' \dontrun{
#' pmxOptions(myOption = 10)
#' getPmxOption("myOption")
#' }
#' @export
pmxOptions <- function(...) {
  newOpts <- list(...)
  
  if (length(newOpts) > 0) {
    .globals$options <- dropNulls.(mergeVectors.(.globals$options, newOpts))
    invisible(.globals$options)
  } else {
    .globals$options
  }
}


checkPmxOption <- function(value, pmxname){
  call <- match.call()
  if(missing(value))
    value <- getPmxOption(pmxname)
  if(is.null(value))
    stop(
      sprintf("Please set a %s argument or set global %s option", 
              deparse(call$value), pmxname)
    )
  value
}

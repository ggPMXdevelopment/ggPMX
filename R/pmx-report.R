

#' Creates a standing report
#'
#' @param name \code{charcter} report name
#'
#' @return a new folder containg standing report template
#' @export
#' @importFrom rmarkdown draft
#'
#' @examples
#' pmx_standing_report("my_report")
pmx_standing_report <-
  function(name){
    draft(sprintf("%s.Rmd",name), template = "standing", package = "reportPMX")
  }

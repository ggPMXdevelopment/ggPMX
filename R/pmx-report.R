#' Creates a standing report
#'
#' @param name \code{charcter} report name
#'
#' @return a new folder containg standing report template
#' @export
#' @importFrom rmarkdown draft render
#'
#' @examples
#' pmx_standing_report("my_report")
pmx_report <-
  function(name,template="standing",render=TRUE){
    file_name <- sprintf("%s.Rmd",name)
    if(file.exists(file_name))
      file.remove(file_name)
    draft(
      file_name,
      template = template, 
      package = "ggPMX",
      create_dir=FALSE,
      edit=FALSE
    )
    if(render) render(file_name,"pdf_document")
  }

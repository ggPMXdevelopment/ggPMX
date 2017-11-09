#' Creates a pdf document from a pre-defined template
#'
#' @param name \code{character} report name
#' @param template \code{character} report template
#' @param render \code{logical} if TRUE generate pdf report
#' @param output_dir Output directory. 
#' An alternate directory to write the output file to 
#' (defaults to the directory of the input file).
#' @param ctr controller
#'
#' @export
#' @importFrom rmarkdown draft render
#' @details 
#' \code{pmx_report} uses pre-defined template .Rmd to generate the report. 
#' The idea is to pass the controller as a report argument using knitr \code{params} artifact. 
#' If render=TRUE , it will generate the pdf report and the template used to create that report. 
#' The user can choose to store both in output_dir. 
#' @examples 
#' \dontrun{
#' ctr <- pk_occ() 
#' ctr %>% pmx_report(name="1_popPK_model",template = "all")
#' ctr %>% pmx_report(name="1_popPK_model",template = "standing",output_dir="/tmp")
#' }
pmx_report <-
  function(ctr, name, output_dir=NULL,template="all", render=TRUE) {
    
    file_name <- file.path(output_dir,sprintf("%s.Rmd", name))
    if (file.exists(file_name)) {
      file.remove(file_name)
    }
    draft(
      file_name,
      template = template,
      package = "ggPMX",
      create_dir = FALSE,
      edit = FALSE
    )
    if (render) {
      render(file_name, "pdf_document", params = list(ctr = ctr), envir = new.env(),
             output_dir=output_dir)
    }
  }

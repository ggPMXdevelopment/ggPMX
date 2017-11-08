#' Creates a standing report
#'
#' @param name \code{character} report name
#' @param template \code{character} report template
#' @param render \code{logical} if TRUE generate pdf report
#' @param output_dir Output directory. 
#' An alternate directory to write the output file to 
#' (defaults to the directory of the input file).
#' @param ctr controller
#'
#' @return a new folder containg standing report template
#' @export
#' @importFrom rmarkdown draft render
#'
#' @example inst/examples/report.R
#' 
pmx_report <-
  function(ctr, name, output_dir=NULL,template="standing", render=TRUE) {
    
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

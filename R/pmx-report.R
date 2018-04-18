#' Creates a pdf document from a pre-defined template
#'
#' @param name \code{character} report name
#' @param template \code{character} report template
#' @param render \code{logical} TRUE to generate the report in all formats defined in the template.
#' @param edit \code{logical}  TRUE to edit the template immediately
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
  function(ctr, 
           name, 
           save_dir=NULL,
           output_type=c("plots","report","both"),
           template="standing", 
           footnote=output_type =="both",
           extra.footnote="",
           edit=FALSE
           
  ){
    
    if (!is.null(save_dir)){
      if(!dir.exists(save_dir)){
        stop(sprintf("please provide a valid save directory : %s",save_dir))
      }
      ctr$save_dir <- tools::file_path_as_absolute(save_dir)
    } 
    ctr$footnote <- footnote
    template_file <- file.path(ctr$save_dir, sprintf("%s.Rmd", name))
    if (length(template_file) > 0 && file.exists(template_file)) {
      file.remove(template_file)
    }
    res <- draft(
      template_file,
      template = template,
      package = "ggPMX",
      create_dir = FALSE,
      edit = edit
    )
    
    standalone <- output_type %in% c("plots","both")
    footnote <- output_type == "both" || footnote
    clean <- !standalone
    render <- TRUE
    if (render) {
      render(
        res, "all", params = list(ctr = ctr), envir = new.env(),
        output_dir = save_dir,clean=clean,quiet=TRUE
      )
    }
    
    if(!clean){
      create_ggpmx_gof(ctr$save_dir,name)
      remove_reports(output_type ,ctr$save_dir)
    }
    
    invisible(file.remove(list.files(pattern="[.]md$|[.]tex$",path=ctr$save_dir,full=TRUE)))
    
  }


remove_reports <- function(output_type,save_dir){
  if(output_type=="plots"){
    invisible(file.remove(list.files(pattern=".pdf$|.docx$",path=save_dir,full=TRUE)))
  }
  
}

create_ggpmx_gof <- function(save_dir,name){
  plot_dir <- sprintf("%s_files",name)
  if(dir.exists(file.path(save_dir,plot_dir))){
    out_ <- file.path(save_dir,"ggpmx_GOF")
    rm_dir(out_)
    dir.create(out_)
    in_ <-  file.path(save_dir,plot_dir)
    plots_ <- list.files(in_,recursive = TRUE,full.names = TRUE)
    dest_plots <- gsub("[-]\\d+[.]",".",basename(plots_))
    file.copy(plots_,file.path(out_,dest_plots))
    rm_dir(in_)
  }
}
rm_dir <- function(to_remove){
  if(dir.exists(to_remove)){
    system(sprintf("rm -r %s",to_remove))
  }
  
}

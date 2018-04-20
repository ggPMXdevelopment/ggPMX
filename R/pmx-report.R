#' Generates ggpmX report from a pre-defined template
#'
#' @param ctr controller
#' @param name \code{character} The report name
#' @param output_type \code{character} the result type, can be \cr
#' a standalone directory of plots or a report document as defined in the template \cr
#' (pdf, docx,..) ,or both
#' @param template \code{character} ggPMX predefined template or the path to a custom rmarkdwon template.
#' @param save_dir Output directory.
#' A directory to write the results files to
#' (defaults to the directory of the input file).
#' @param footnote \code{logical}  TRUE to add a footnote to the generated plots. The default footnote is to add \cr 
#' the path where the plot is saved.
#' @param edit \code{logical}  TRUE to edit the template immediately
#'
#' @export
#' @importFrom rmarkdown draft render
#' @details
#' \code{pmx_report} uses pre-defined template .Rmd to generate the report.
#' The idea is to pass the controller as a report argument using knitr \code{params} artifact.
#' @example inst/examples/pmx_report.R

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
    render(
      res, "all", params = list(ctr = ctr), envir = new.env(),
      output_dir = save_dir,clean=clean,quiet=TRUE
    )
    
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

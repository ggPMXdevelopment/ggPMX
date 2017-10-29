#' Creates a standing report
#'
#' @param name \code{character} report name
#' @param template \code{character} report template 
#' @param render \code{logical} if TRUE generate pdf report
#'
#' @return a new folder containg standing report template
#' @export
#' @importFrom rmarkdown draft render
#'
#' @examples
#' \dontrun{
#' c.name <-  "1_popPK_model"
#' data_file <-  "PKdata_ggPMX.csv"
#' uc.dir <- system.file(package = "ggPMX", "testdata",uc.name)
#' wd.mlx <- file.path(uc.dir, "Monolix")
#' input_file <- file.path(uc.dir, data_file)
#' params <- 
#' list(
#' config="standing",
#' directory = wd.mlx, 
#' input = input_file, 
#' dv = "DV", 
#' dvid = "ytype", 
#' cats = c("SEX","RACE","DISE","ILOW"), 
#' conts = c("AGE0","WT0","HT0","TRT"),
#' occ="ISS"
#' )
#' ctr <- do.call(pmx_mlx,params)
#' ctr %>% pmx_report("1_popPK_model","all")
#' }
#' 
pmx_report <-
  function(ctr,name,template="standing",render=TRUE){
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
    if(render) 
      if(is.null(params)) render(file_name,"pdf_document",envir = new.env())
    else render(file_name,"pdf_document",params=list(ctr=ctr),envir = new.env()) 
  }


#' Add draft layer annotation
#'
#' This function adds the word draft to certain graphics.
#' @param label draft layer default to DRAFT
#' @param size size of the annotation
#' @param color color of the annotation default to grey50
#' @param x \code{numeric} x coordinate of the draft label 
#' @param y \code{numeric} y coordinate of the draft label 
#'
#' @return ggplot2 annotation
#' @export
#'
#' @examples
#' add_draft("DRAFT", size = 5, color = "grey50")
add_draft <- function(label = "DRAFT", size=10, color="grey50",x = Inf, y = -Inf){
  ggplot2::annotate('text', label = label, size = size,
           colour = color, family = 'Courier',
           x = x, y = y,
            hjust = 1.2 , vjust = -1.2)
}

#' Give the whole abbreviation definition
#'
#' @param param abbreviation term
#'
#' @return characater abbreviation defintion
#' @export
#' @examples
#' abbrev("VPC")
abbrev <- function(param) {
  keys_file <- file.path(system.file(package = "ggPMX"), "init", 
                         "abbrev.yaml")
  keys <- yaml.load_file(keys_file)
  if(missing(param)) keys
  else  keys[[param]]
}


#' @importFrom data.table fread
pmx_fread <- function(...){
  fread(na.strings = c("NA","."),...)
}

is.formula <- function(x){
  inherits(x,"formula")
}


#' Creates pmx controller using default package data
#'
#' @return pmx controller
#' @export
#'
#' @examples
#' pmx_ctr()
pmx_ctr <- function(){
  theophylline <- file.path(system.file(package = "ggPMX"), "testdata", 
                            "theophylline")
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  
  pmx_mlx(
    config = "standing", 
    directory = WORK_DIR, 
    input = input_file, dv = "Y", 
    dvid ="DVID")
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
#' Merge 2 lists
#'
#' left join , the first list is updated by the seond one
#' @param base_list  list to update
#' @param overlay_list list used to update the first list
#' @param recursive logical if TRUE do the merge in depth
#'
#' @return list
l_left_join <-
  function (base_list, overlay_list, recursive = TRUE)
  {
    if (length(base_list) == 0)
      overlay_list
    else if (length(overlay_list) == 0)
      base_list
    else {
      merged_list <- base_list
      for (name in names(overlay_list)) {
        base <- base_list[[name]]
        overlay <- overlay_list[[name]]
        if (is.list(base) && is.list(overlay) && recursive)
          merged_list[[name]] <- l_left_join(base, overlay)
        else {
          merged_list[[name]] <- NULL
          merged_list <-
            append(merged_list,
                   overlay_list[which(names(overlay_list) %in%
                                        name)])
        }
      }
      merged_list
    }
  }


## shiny:::anyUnnamed
anyUnnamed. <- function (x)
{
  if (length(x) == 0)
    return(FALSE)
  nms <- names(x)
  if (is.null(nms))
    return(TRUE)
  any(!nzchar(nms))
}

## shiny:::mergeVectors
mergeVectors. <- function (a, b)
{
  if (anyUnnamed.(a) || anyUnnamed.(b)) {
    stop("Vectors must be either NULL or have names for all elements")
  }
  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}
## shiny:::dropNulls
dropNulls. <-
function (x)
{
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


local_filter <- 
  function(pmx_exp){
    
    e <- if(is.character(pmx_exp))parse(text=pmx_exp)
    else as.expression(pmx_exp)
    filter_ <-function(x){
      r <- try(eval(parse(text = e), x),silent=TRUE)
      if(inherits(r,"try-error")) r <- TRUE
      x[r & !is.na( r )]
    }
  }


merge_defaults <- 
function (x, y) 
{
  c(x, y[setdiff(names(y), names(x))])
}




#' Add draft layer annotation
#'
#' This function adds the word draft to certain graphics.
#' @param label draft layer default to DRAFT
#' @param size size of the annotation
#' @param color color of the annotation default to grey50
#' @param x \code{numeric} x coordinate of the draft label 
#' @param y \code{numeric} y coordinate of the draft label 
#' @param ... extra paremeters to geom text used to annotate the draft
#'
#' @return ggplot2 annotation
#' @export
#'
#' @examples
#' add_draft("DRAFT", size = 5, color = "grey50")
add_draft <- function(label = "DRAFT", size=10, color="grey50",x = Inf, y = -Inf,...){
  do.call(annotate,list(geom='text', label = label, size = size,
                        colour = color, family = 'Courier',
                        x = x, y = y,
                        hjust = 1.2 , vjust = -1.2,...))
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


#' @import data.table
pmx_fread <- function(...){
  fread(na.strings = c("NA","."),...)
}

is.formula <- function(x){
  inherits(x,"formula")
}


#' Creates pmx controller using theophylline data
#'
#' @return pmx controller
#' @export
#'
#' @examples
#' theophylline()
theophylline <- function(){
  theophylline <- file.path(system.file(package = "ggPMX"), "testdata", 
                            "theophylline")
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  
  pmx_mlx(
    config = "standing", 
    directory = WORK_DIR, 
    input = input_file, 
    dv = "Y", 
    dvid ="DVID",
    cats=c("SEX"),
    conts=c("WT0","AGE0"),
    strats="STUD")
}


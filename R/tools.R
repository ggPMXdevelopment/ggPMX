
#' Add draft layer annotation
#'
#' This is add the word draft to certain graphics.
#' @param label draft layer default to DRAFT
#' @param size size of the annotation
#' @param color color of the annotation default to grey50
#'
#' @return ggplot2 annotation
#' @export
#'
#' @examples
#' add_draft("DRAFT",size=5,color="grey50")
add_draft <- function(label = "DRAFT", size, color){
  ggplot2::annotate('text', label = label, size = size,
           colour = color, family = 'Courier',
           x = Inf, y = -Inf, hjust = 1.2 , vjust = -1.2)
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

#' Shiny application to test ggPMX plots
#'
#' @return This function does not return. It is a wrapper for runApp. 
#' Interrupt R to stop the application (usually by pressing Ctrl+C or Esc).
#' @export
plotTester <- function() {
  appDir <- system.file("shiny-helpers", "plotting-app", 
                        package = "ggPMX")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ggPMX`.", 
         call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

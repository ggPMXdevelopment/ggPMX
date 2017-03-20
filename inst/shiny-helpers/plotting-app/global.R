library(shiny)
library(ggPMX)
library(ggplot2)
source_monolix_data <- function(){
  monolix_dir <- Sys.getenv("PMXTESTDATA")
  if(identical(monolix_dir, "")){
    stop("Please set an environment variable PMXTESTDATA containing", 
         " monolix RESULTS directories")
  }
  result_dirs_PKPD <- grep("RESULTS", 
                           list.dirs(file.path(monolix_dir, "PKPD"),
                                     full.names = TRUE), 
                           value = TRUE)
  result_dirs_PK <- grep("RESULTS", 
                         list.dirs(monolix_dir, full.names = TRUE), 
                         value = TRUE)
  result_dirs_PK <- result_dirs_PK[!result_dirs_PK %in% result_dirs_PKPD]
  if(length(c(result_dirs_PK, result_dirs_PKPD)) == 0){
    stop(monolix_dir, " contains no RESULTS directories")
  }
  label_dirs <- function(result_dirs){
    results_labels <- vapply(result_dirs, function(x){
      basename(dirname(x))
    }, character(1), USE.NAMES = FALSE)
    setNames(result_dirs, results_labels)
  }
  lapply(list(PK = result_dirs_PK, PKPD = result_dirs_PKPD), label_dirs)
}

monolix_data <- source_monolix_data()
monolix_data[["PK"]][["PKGDATA"]] <- 
  WORK_DIR <- file.path(system.file(package="ggPMX"), "testdata", 
                        "theophylline", "Monolix")
pmxOptions(work_dir = monolix_data[["PK"]][[1]])
ctr <- pmx_mlx("standing")
dataset <- diamonds

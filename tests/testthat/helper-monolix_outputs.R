source_monolix_data <- function(){
  monolix_dir <- Sys.getenv("PMXTESTDATA")
  if(identical(monolix_dir, "")){
    stop("Please set an environment variable PMXTESTDATA containing", 
         " monolix RESULTS directories")
  }
  result_dirs <- grep("RESULTS", 
                      list.dirs("~/pmx_data", full.names = TRUE, 
                                recursive = TRUE), 
                      value = TRUE)
  if(length(result_dirs) == 0){
    stop(monolix_dir, " contains no RESULTS directories")
  }
  results_labels <- vapply(result_dirs, function(x){
    basename(dirname(x))
  }, character(1), USE.NAMES = FALSE)
  setNames(result_dirs, results_labels)
}

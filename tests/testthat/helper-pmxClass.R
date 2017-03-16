test_pmxClass_helpers <- function(){
  WORK_DIR <- file.path(system.file(package="ggPMX"), "testdata", 
                        "theophylline", "Monolix")
  pmxOptions(work_dir = WORK_DIR)
  list(workdir = WORK_DIR)
}

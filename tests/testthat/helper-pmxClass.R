test_pmxClass_helpers <- function(){
  theophylline <- file.path(system.file(package = "ggPMX"), "testdata",
                            "theophylline")
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  pmxOptions(
    work_dir = WORK_DIR,
    input = input_file,
    dv = "Y"
  )
  list(workdir = WORK_DIR, input = input_file)
}

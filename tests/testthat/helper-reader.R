
reader_helpers <- function(){
  WORK_DIR <- file.path(system.file(package="ggPMX"), "testdata", 
                        "theophylline", "Monolix")
  testconf <- load_config("standing", "mlx")
  list(wd = WORK_DIR, conf = testconf)
}


reader_helpers <- function() {
  theophylline <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "theophylline"
  )
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  testconf <- load_config("standing", "mlx")
  list(wd = WORK_DIR, input = input_file, conf = testconf)
}

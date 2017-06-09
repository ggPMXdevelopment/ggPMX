.onLoad <- function(libname = find.package("ggPMX"), pkgname = "ggPMX"){

  pmxOptions(template_dir = file.path(libname, pkgname, "templates"))
  theophylline <- file.path(libname, pkgname,"testdata", "theophylline")
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  pmxOptions(work_dir = WORK_DIR, input = input_file, dv = "Y")
}

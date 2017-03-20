.onLoad <- function(libname = find.package("ggPMX"), pkgname = "ggPMX"){

  pmxOptions(work_dir = "/home/agstudy")

  pmxOptions(template_dir = file.path(libname, pkgname, "templates"))


}

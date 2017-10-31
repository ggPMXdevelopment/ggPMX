.onLoad <- function(libname = find.package("ggPMX"), pkgname = "ggPMX") {
  pmxOptions(template_dir = file.path(libname, pkgname, "templates"))
}

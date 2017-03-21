helper_updateplots <- function(){
  WORK_DIR <- file.path(system.file(package="ggPMX"), "testdata", "theophylline", "Monolix")
  pmxOptions(work_dir=WORK_DIR)
  ctr <- pmx_mlx(config = "standing")
  list(ctr = ctr, wd = WORK_DIR)
}

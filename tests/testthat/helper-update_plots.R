helper_updateplots <- function() {
  theophylline <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "theophylline"
  )
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  ctr <- pmx_mlx(
    config = "standing", directory = WORK_DIR,
    input = input_file,
    dv = "Y", dvid = "DVID",
    conts = c("WT0", "AGE0"),
    cats = c("SEX"),
    occ = "",
    strats = c("STUD"),
    settings=pmx_settings(use.titles = TRUE)
  )
  list(ctr = ctr, wd = WORK_DIR, input = input_file)
}

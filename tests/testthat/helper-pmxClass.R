test_pmxClass_helpers <- function() {
  theophylline <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "theophylline"
  )
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  ctr <- pmx_mlx(
    "standing",
    directory = WORK_DIR,
    input = input_file,
    dv = "Y",
    dvid = "DVID",
    conts = c("WT0", "AGE0"),
    cats = c("SEX"),
    strats = c("STUD")
  )
  list(
    workdir = WORK_DIR,
    input = input_file,
    ctr = ctr
  )
}

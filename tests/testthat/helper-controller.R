test_controller <- function(plots = character(), settings = NULL, bloq = NULL) {
  config <- load_config("standing", "mlx")
  stopifnot(all(plots %in% names(config$plots)))
  # Data-only tests do not need every default plot constructed eagerly.
  config$plots <- config$plots[plots]
  directory <- system.file("testdata", "theophylline", package = "ggPMX")
  pmx_mlx(
    config = config,
    directory = file.path(directory, "Monolix"),
    input = file.path(directory, "data_pk.csv"),
    dv = "Y",
    dvid = "dvid",
    cats = "SEX",
    conts = c("WT0", "AGE0"),
    strats = "STUD",
    settings = settings,
    sim = NULL,
    bloq = bloq
  )
}


# basic example  ----------------------
#' \dontrun{
uc.name <- "1_popPK_model"
data_file <- "PKdata_ggPMX.csv"
uc.dir <- system.file(package = "ggPMX", "testdata",uc.name)
wd.mlx <- file.path(uc.dir, "Monolix")
input_file <- file.path(uc.dir, data_file)
params <-
  list(
    config="standing",
    directory = wd.mlx,
    input = input_file,
    dv = "DV",
    dvid = "YTYPE",
    cats = c("SEX","RACE","DISE","ILOW"),
    conts = c("AGE0","WT0","HT0","TRT"),
    occ="ISS"
  )
ctr <- do.call(pmx_mlx,params)
ctr %>% pmx_report("1_popPK_model","all")
#'}

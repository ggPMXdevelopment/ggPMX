\donttest{

## Example to create the controller using theophylline data
theophylline <- file.path(system.file(package = "ggPMX"), "testdata",
                          "theophylline")
WORK_DIR <- file.path(theophylline, "Monolix")
input_file <- file.path(theophylline, "data_pk.csv")

## using only mandatory variables
ctr <- pmx(
  sys="mlx",
  config = "standing",
  directory = WORK_DIR,
  input = input_file,
  dv = "Y",
  dvid ="DVID"
)
## Using covariates
ctr <- pmx(
  sys="mlx",
  config = "standing",
  directory = WORK_DIR,
  input = input_file,
  dv = "Y",
  dvid ="DVID",
  cats=c("SEX"),
  conts=c("WT0","AGE0"),
  strats="STUD"
)
## using settings parameter
ctr <- pmx(
  sys="mlx",
  config = "standing",
  directory = WORK_DIR,
  input = input_file,
  dv = "Y",
  dvid ="DVID",
  settings=list(is.draft=FALSE)
)

## using mlxtran file
mlxtran_file <- 
  file.path(system.file(package = "ggPMX"), 
    "testdata","1_popPK_model","project.mlxtran")
pmx_mlxtran(mlxtran_file)

## mlxtran , call =TRUE to get the pmx_mlx argument parsed by  pmx_mlxtran
params <- pmx_mlxtran(mlxtran_file,call=TRUE)

str(params)
# $ directory: chr results_pathile 
# $ input    : chr observation file path
# $ dv       : chr "DV"
# $ cats     : chr [1:4] "SEX" "RACE" "DISE" "ILOW"
# $ conts    : chr [1:4] "AGE0" "WT0" "HT0" "TRT"
# $ occ      : chr "ISS"
# $ dvid     : chr "YTYPE"
# $ endpoint :List of 5
# ..$ code     : chr "1"
# ..$ label    : chr ""
# ..$ unit     : chr ""
# ..$ file.code: chr "1"
# ..$ trans    : NULL
# ..- attr(*, "class")= chr "pmxEndpointClass"
# $ config   : chr "standing"
}

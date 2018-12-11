## \dontrun{

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
mlxtran_file <- file.path(system.file(package = "ggPMX"), "testdata","1_popPK_model","project.mlxtran")
pmx_mlxtran(mlxtran_file)


## }

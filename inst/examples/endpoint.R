\donttest{
## Use file.code parameter
pk_pd_path <- file.path(system.file(package = "ggPMX"), "testdata","pk_pd")

WORK_DIR <- file.path(pk_pd_path, "RESULTS")

ep <- pmx_endpoint(
  code="4",
  file.code="2"
)

input_file <- file.path(pk_pd_path, "pk_pd.csv")

ctr <- pmx_mlx(
  config = "standing",
  directory = WORK_DIR,
  input = input_file,
  dv = "dv",
  dvid = "dvid",
  cats = "sex",
  conts = "wt",
  endpoint = ep
 )

## using mlxtran 

ep <- pmx_endpoint(
  code="3",
 file.code="1"
)

mlxtran_file <- file.path(pk_pd_path, "pk_pd.mlxtran")
ctr <- pmx_mlxtran(mlxtran_file,endpoint=ep)
}

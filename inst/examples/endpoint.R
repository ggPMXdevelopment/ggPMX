
## use custom finegrid and predictions 
pk_pd_path <- file.path(system.file(package = "ggPMX"), "testdata","pk_pd")

WORK_DIR <- file.path(pk_pd_path, "RESULTS")

ep <- pmx_endpoint(
  code="4",
  predictions="predictions2",
   finegrid="finegrid2"
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
  predictions="predictions1",
  finegrid="finegrid1"
)

mlxtran_file <- file.path(pk_pd_path, "pk_pd.mlxtran")
ctr <- pmx_mlxtran(mlxtran_file,endpoint=ep)

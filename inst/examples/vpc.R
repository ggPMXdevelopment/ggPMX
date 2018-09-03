
library(ggPMX)

theophylline <- file.path(
  system.file(package = "ggPMX"), "testdata",
  "theophylline"
)
WORK_DIR <- file.path(theophylline, "Monolix")
input_file <- file.path(theophylline, "data_pk.csv")
vpc_file <- file.path(theophylline, "VPC_data.csv")

ctr <- pmx_mlx(
  config = "standing",
  directory = WORK_DIR,
  input = input_file,
  dv = "Y",
  dvid = "dvid",
  cats = c("SEX"),
  conts = c("WT0", "AGE0"),
  strats = "STUD",
  sim = pmx_sim(
    
    file = vpc_file,
    irun ="rep",
    idv="TIME",
    dv ="Y"
  )
)


ctr %>% pmx_plot_vpc(
  type="percentile",
  is.draft = FALSE,
  pi = pmx_pi(interval = c(0.05,0.95)),
  ci = pmx_pi(interval = c(0.05,0.95))
)


library(ggPMX)
ctr <- pmx(
  config = "standing",
  sys = "mlx",
  input = "~/Downloads/obs.csv",
  directory = "~/Downloads",
  dv ="y",
  sim = pmx_sim(
    
    file = "~/Downloads/sim.csv",
    irun ="stu",
    idv="TIME",
    dv ="Cc"
  )
)

ctr %>% pmx_plot_vpc(
  type="percentile",
  is.draft = FALSE,
  bin=pmx_bin(style="hclust",n=6),
  pi = pmx_pi(interval = c(0.1,0.90)),
  ci = pmx_pi(interval = c(0.1,0.90)),
  rug=NULL
)
 


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

ctr %>% pmx_plot_vpc(type ="percentile",
                     bin=pmx_bin(style="quantile",n=5),
                     obs=pmx_obs(
                       color = "green",shape = 20,size=1))

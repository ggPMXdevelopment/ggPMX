# 
# 
# 
# ctr <- pmx(
#   config = "standing",
#   sys = "mlx",
#   input = "~/Downloads/obs.csv",
#   directory = "~/Downloads",
#   dv ="y",
#   sim = pmx_sim(
#     file = "~/Downloads/sim.csv",
#     irun ="stu",
#     idv="TIME",
#     dv ="Cc"
#     )
# )
# 
# ctr %>% pmx_plot_vpc(type ="percentile")
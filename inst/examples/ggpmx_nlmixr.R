


library(nlmixr)

# one.compartment <- function() {
#   ini({
#     tka <- 0.45 # Log Ka
#     tcl <- 1 # Log Cl
#     tv <- 3.45    # Log V
#     eta.ka ~ 0.6
#     eta.cl ~ 0.3
#     eta.v ~ 0.1
#     add.sd <- 0.7
#   })
#   model({
#     ka <- exp(tka + eta.ka)
#     cl <- exp(tcl + eta.cl)
#     v <- exp(tv + eta.v)
#     d/dt(depot) = -ka * depot
#     d/dt(center) = ka * depot - cl / v * center
#     cp = center / v
#     cp ~ add(add.sd)
#   })
# }
# 
# fit <- nlmixr(one.compartment, theo_sd, "saem", control=list(print=0))
library(ggPMX)
ctr <- pmx_nlmixr(fit, conts = c("cl","v"))

ctr %>% pmx_plot_dv_ipred
ctr %>% pmx_plot_dv_pred
ctr %>% pmx_plot_abs_iwres_ipred
ctr %>% pmx_plot_individual(1)
ctr %>% pmx_plot_iwres_dens
ctr %>% pmx_plot_npde_qq
ctr %>% pmx_plot_npde_pred
ctr %>% pmx_plot_npde_time
ctr %>% pmx_plot_eta_qq
ctr %>% pmx_plot_vpc
ctr %>% pmx_plot_eta_box
ctr %>% pmx_plot_eta_hist
ctr %>% pmx_plot_eta_matrix

ctr %>% pmx_report("nlmixr_report","/tmp")

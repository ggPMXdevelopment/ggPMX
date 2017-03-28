## ----load_pacakge, echo=FALSE--------------------------------------------

library(ggPMX)
#WORK_DIR <- "../ggpmx_files/inputs"
theophylline <- file.path(system.file(package = "ggPMX"), "testdata", 
                          "theophylline")
WORK_DIR <- file.path(theophylline, "Monolix")
input_file <- file.path(theophylline, "data_pk.csv")



## ----list_configs--------------------------------------------------------
configs()

## ----init_controller-----------------------------------------------------
ctr <- pmx(config = "standing", sys = "mlx", directory = WORK_DIR, 
           input = input_file, dv = "Y")

## ----init_work_dir-------------------------------------------------------
pmxOptions(
  work_dir = WORK_DIR, 
  input = input_file, 
  dv = "Y")
ctr <- pmx_mlx(config = "standing")


## ---- eval=FALSE---------------------------------------------------------
#      label : model predictions
#      file  : MLX_predictions.txt
#      reader  : read_mlx_pred
#      names:
#        id: ID
#        time: TIME
#        y1: DV
#        poppred: PRED
#        npde: NPDE

## ----load_conf-----------------------------------------------------------

conf <- load_config("standing", sys = "mlx")
conf


## ----change_mapping------------------------------------------------------

conf$data$mod_pred$names$time <- NULL
conf$data$mod_pred$names$time1 <- "TIME"


## ----change_mapping_future, eval=FALSE-----------------------------------
#  
#  # The following does not run - TODO: Fix it
#  #ctr %>% rename("time","time1")
#  #ctr %>% rename("mod_pred","time","time1")
#  

## ---- distribut----------------------------------------------------------
library(ggPMX)

ctr %>% set_plot("DIS", pname = "distr1", type = "box")

ctr %>% get_plot("distri")

ctr %>% get_plot("distr1")


## ----complete_example, warning=FALSE-------------------------------------
library(ggPMX)

## define a working dir
# Example that points to file in user's work directory
# Commented because the vignette will not build if it is on:
# pmxOptions(work_dir="/home/agstudy/projects/r/ggPMX/ggPMX_files/inputs")



#WORK_DIR <- "../ggpmx_files/inputs"
theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
WORK_DIR <-  file.path(theophylline, "Monolix")
input_file <- file.path(theophylline,"data_pk.csv")
pmxOptions(
  work_dir=WORK_DIR,
  input=input_file,
  dv="Y")

ctr <- pmx_mlx(config = "standing")


## add a new plot
ctr %>%
   set_plot(ptype="DIS",
            pname="distri_box",
            type="box",
            has.shrink=TRUE,
            has.jitter=FALSE)

## return all plots
lapply(ctr %>% plot_names,
       function(x){
        if(x=="indiv") return(ctr%>%get_plot(x, c(2, 4)))
         ctr %>% get_plot(x)
       })





## ----load_package, echo=FALSE--------------------------------------------
opts_chunk$set(out.width='100%')

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

conf$data$predictions$names$time <- NULL
conf$data$predictions$names$time1 <- "TIME"


## ----change_mapping_future, eval=FALSE-----------------------------------
#  
#  # The following does not run - TODO: Fix it
#  #ctr %>% rename("time","time1")
#  #ctr %>% rename("predictions","time","time1")
#  

## ---- eval=FALSE---------------------------------------------------------
#  library(ggPMX)
#  theophylline <- file.path(system.file(package = "ggPMX"), "testdata",
#                            "theophylline")
#  WORK_DIR <- file.path(theophylline, "Monolix")
#  input_file <- file.path(theophylline, "data_pk.csv")
#  pmxOptions(
#    work_dir = WORK_DIR,
#    input = input_file,
#    dv = "Y")
#  ctr <- pmx_mlx(config = "standing")
#  

## ----pmx_gpar_args-------------------------------------------------------
args(pmx_gpar)

## ---- distribut,  fig.width=7, fig.height=6, warning=FALSE---------------
library(ggPMX)

ctr %>% set_plot("DIS", pname = "distr1", type = "box")

ctr %>% get_plot("distr1")


## ---- individual_plot,  fig.width=7, fig.height=6, warning=FALSE---------
library(ggPMX)

ctr %>% set_plot("IND", pname = "indiv1", 
                 draft = list(size = 10, label = "DRAFT", color = "grey50"))

ctr %>% get_plot("indiv1", c(2, 4))


## ---- residual_plot,  fig.width=7, fig.height=6--------------------------
library(ggPMX)

ctr %>% set_plot("RES", pname = "res1", y = "IWRES", x = "IPRED")

ctr %>% get_plot("res1")


## ----complete_example, warning=FALSE,  fig.width=7, fig.height=6---------
library(ggPMX)

## define a working dir
# Example that points to file in user's work directory
# Commented because the vignette will not build if it is on:
# pmxOptions(work_dir="/home/agstudy/projects/r/ggPMX/ggPMX_files/inputs")

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

## update individual plots draft argument
ctr %>% pmx_update("indiv", 
                   draft = list(size = 10, label = "DRAFT", 
                                color = "grey50"))

## return all plots
lapply(ctr %>% plot_names,
       function(x){
        if(x=="indiv") return(ctr%>%get_plot(x, c(2, 4)))
         ctr %>% get_plot(x)
       })




## ----pmx_filter,  fig.width=7, fig.height=6------------------------------
plotnames <- ctr %>% plot_names()
ctr %>% 
  pmx_filter(data_set = "eta", ID <= 5) %>% 
  get_plot(plotnames[1])
  


## ----update_plot,  fig.width=7, fig.height=6-----------------------------

library(ggPMX)

ctr %>% set_plot("RES", pname = "res1", y = "IWRES", x = "IPRED")

p1 <- ctr %>% get_plot("res1")

# remove bands
p2 <- ctr %>% 
  pmx_update("res1", has.band = FALSE, 
             labels = list(title = "IPRED versus IWRES (no bands)")) %>% 
  get_plot("res1")

p1
p2


## ---- include=FALSE------------------------------------------------------
library(ggPMX)
library(knitr)
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 4
)


## ---- results="asis", echo=TRUE------------------------------------------
library("ggPMX")

# Load standard configuration
theophylline <- file.path(system.file(package = "ggPMX"), "testdata", 
                          "theophylline")
WORK_DIR <- file.path(theophylline, "Monolix")
input_file <- file.path(theophylline, "data_pk.csv")
ctr <- pmx(
  config = "standing", sys = "mlx", 
  directory = WORK_DIR, 
  input = input_file, 
  dv = "Y", 
  dvid ="DVID")

## ---- results="asis", echo=TRUE------------------------------------------
# TODO: Check if this step makes sense as a standalone (--> move to misc if not)

## ---- results="asis", echo=TRUE------------------------------------------
lapply(ctr %>% plot_names,
       function(x){
         if(x=="individual") 
           return(ctr %>% get_plot(x, c(2, 4)))
         ctr %>% get_plot(x)
       })

## ---- results="asis", echo=TRUE------------------------------------------
ctr4 <- pmx(
  config = "standing", sys = "mlx", 
  directory = WORK_DIR, 
  input = input_file, 
  dv = "Y", 
  dvid ="DVID",
  cats="SEX",
  settings=
    pmx_settings(
      cats.labels=list(
        SEX=c("0"="M","1"="F"),
        STUD=c("1"="Study 1","2"="Study 2")
      ),
      use.labels = TRUE
    )
)

## default plot
ctr4 %>% get_plot("npde_pred")

## Set aesthetics (colours, size, etc.) to fixed value?
ctr4 %>% pmx_update("npde_pred", point=list(shape=3,color="green",size=3))
ctr4 %>% get_plot("npde_pred")

## Set aesthetics (colours, size, etc.) to data column?
ctr4 %>% pmx_update("npde_pred", point=list(aes(shape="SEX")))
ctr4 %>% get_plot("npde_pred")
##??stop("Does not produce desired result with shape defined by SEX")

# alternative - use strat.color 
ctr4 %>% pmx_update("npde_pred", 
                    point=list(shape=1,color="black",size=1), 
                    strat.color="SEX")
ctr4 %>% get_plot("npde_pred")
##stop("Does not produce desired result with shape defined by SEX")

## Add/remove layers?
ctr4 %>% pmx_update("npde_pred", is.smooth=FALSE, is.band=FALSE, is.identity_line=TRUE, strat.color=NULL)
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", is.smooth=TRUE, is.band=TRUE, is.identity_line=FALSE)
ctr4 %>% get_plot("npde_pred")

## Add/remove facetting?
# add
ctr4 %>% pmx_update("npde_pred", strat.facet="SEX")
ctr4 %>% get_plot("npde_pred")
# change
ctr4 %>% pmx_update("npde_pred", strat.facet="STUD")
ctr4 %>% get_plot("npde_pred")
## stop("Error in title")
# remove
ctr4 %>% pmx_update("npde_pred", strat.facet=NULL)
ctr4 %>% get_plot("npde_pred")
# by two variables
ctr4 %>% pmx_update("npde_pred", strat.facet= SEX ~ STUD)
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", strat.facet=NULL)

##log axis?
ctr4 %>% pmx_update("npde_pred", scale_x_log10=TRUE) # ok
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", scale_y_log10=TRUE) # produces expected nonesense result
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", scale_y_log10=FALSE, scale_x_log10=FALSE)
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", trans = "abs_y", log_y=FALSE, log_x=FALSE)
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", trans = "abs_y", log_y=TRUE, log_x=FALSE)
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", trans= "", scale_y_log10=FALSE, scale_x_log10=FALSE)
          
##Subsetting?
ctr4 %>% pmx_update("npde_pred", filter = SEX == 1, strat.facet="SEX")
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", filter = TRUE)
ctr4 %>% get_plot("npde_pred")
ctr4 %>% pmx_update("npde_pred", strat.facet = NULL)
ctr4 %>% get_plot("npde_pred")

##Add/remove draft label?
ctr4 %>% pmx_update("npde_pred", is.draft = FALSE)
ctr4 %>% get_plot("npde_pred")
## stop("Draft label is still present")




## ------------------------------------------------------------------------
sessionInfo()
date()


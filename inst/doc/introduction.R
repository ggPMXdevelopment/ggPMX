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
conf <- load_config("standing", sys="mlx")
conf

# Init controller object from (test) data
ctr <- pmx(conf, directory=file.path(system.file(package="ggPMX"), 
                                     "testdata", "theophylline", "Monolix"))

## ---- results="asis", echo=TRUE------------------------------------------
# TODO: Check if this step makes sense as a standalone (--> move to misc if not)

## ---- results="asis", echo=TRUE------------------------------------------
lapply(ctr %>% plot_names,
       function(x){
         if(x=="indiv") 
           return(ctr %>% get_plot(x, c(2, 4)))
         ctr %>% get_plot(x)
         })

## ------------------------------------------------------------------------
sessionInfo()
date()


## ----load_package, echo=FALSE,warning=FALSE,message=FALSE----------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

## ----load_intro, echo=FALSE,warning=FALSE,message=FALSE------------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
work_dir <- file.path(theophylline, "Monolix")
input_data <- file.path(theophylline, "data_pk.csv")

ctr <- theophylline()

## ----illustrate_diagnostic, out.width='.25\\linewidth', fig.width=4, fig.height=4, fig.show='hold', fig.align='center', echo=FALSE----
ctr <- theophylline()
ctr %>% pmx_plot_dv_pred
ctr %>% pmx_plot_eta_box
ctr %>% pmx_plot_eta_hist(facets=list(scales="free_y"))
ctr %>% pmx_plot_individual(npage = 1,
                            is.legend=FALSE,
                            facets=list(ncol=2,nrow=3))
ctr %>% pmx_plot_eta_matrix(
  shrink=list(size=3,hjust=1.5))

## ----load_archi, echo=FALSE,warning=FALSE,message=FALSE------------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
work_dir <- file.path(theophylline, "Monolix")
input_data <- file.path(theophylline, "data_pk.csv")

ctr <- theophylline()

## ----echo=FALSE, out.width='80%', fig.align='center'---------------------
knitr::include_graphics("./ggPMX_arch.png")

## ----datasets_list,echo=FALSE,results='asis'-----------------------------

out <- rbind(
  c("input", "Input modeling dataset"),
  c("estimates", "Estimated population parameters"),
  c("eta", "Random effects, their standard deviation and residual errors (to calculate shrinkage)"),
  c("predictions", "Observations and predictions at times of observations dataset"),
  c("finegrid", "Additional predictions (at times without observations)")
)

colnames(out) <- c("ggPMX dataset", "Description")
# knitr::kable(out)
# latex(head(out), file='', label='tab:ggPMX_datasets', caption='ggPMX datasets',where = "!htbp")
xt <- xtable(head(out), label = "tab:ggPMX_datasets", caption = "ggPMX datasets")
print(xt, comment = F)

## ----load_basics, echo=FALSE,warning=FALSE,message=FALSE-----------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

## ----init_ctr------------------------------------------------------------

theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
work_dir <- file.path(theophylline, "Monolix")
input_data <- file.path(theophylline, "data_pk.csv")

ctr <- pmx(
  sys = "mlx",
  config = "standing",
  directory = work_dir,
  input = input_data,
  dv = "Y",
  dvid = "DVID"
)

## ----theophylline_ctr----------------------------------------------------
ctr <- theophylline()

## ----display_ctr---------------------------------------------------------
ctr

## ----init_pmx_mlx--------------------------------------------------------
ctr <- pmx_mlx(
  config = "standing",
  directory = work_dir,
  input = input_data,
  dv = "Y",
  dvid = "DVID"
)

## ----init_ctr_covar------------------------------------------------------
ctr <- pmx_mlx(
  config = "standing",
  directory = work_dir,
  input = input_data,
  dv = "Y",
  dvid = "DVID",
  cats = c("SEX"),
  conts = c("WT0", "AGE0"),
  strats = c("STUD", "SEX")
)

## ----get_covar-----------------------------------------------------------
ctr %>% get_cats()
ctr %>% get_conts()
ctr %>% get_strats()
ctr %>% get_covariates()

## ----plot_lists----------------------------------------------------------
ctr %>% plot_names()

## ----plot_types----------------------------------------------------------
ctr %>% plots()

## ----basics_res, out.width='.48\\linewidth', fig.height=4, fig.width=6, fig.show='hold', fig.align='center'----
ctr %>% pmx_plot_dv_pred
ctr %>% pmx_plot_dv_ipred

ctr %>% pmx_plot_iwres_time
ctr %>% pmx_plot_npde_time

ctr %>% pmx_plot_iwres_ipred
ctr %>% pmx_plot_abs_iwres_ipred

ctr %>% pmx_plot_npde_pred

## ----basics_ebe_hist , fig.height=3, fig.width=3, fig.show='hold', fig.align='center'----
ctr %>% pmx_plot_eta_hist()
ctr %>% pmx_plot_eta_box()


## ----basics_indiv, fig.height=6, fig.width=6, fig.show='hold', fig.align='center'----
ctr %>% pmx_plot_individual(npage = 1)

## ----basics_qq, fig.height=3, fig.width=3, fig.show='hold', fig.align='center'----
ctr %>% pmx_plot_npde_qq()
ctr %>% pmx_plot_iwres_qq()

## ----basics_matrix_plot,  fig.height=6, fig.width=6, fig.show='hold', fig.align='center'----
ctr %>% pmx_plot_eta_matrix

## ----basics_update_defaults, out.width='.98\\linewidth', fig.height=6, fig.width=9, fig.show='hold', fig.align='center'----
ctr %>% pmx_plot_individual(
  npage = 1, 
  facets = list(nrow = 2, ncol = 2),
  labels = list(title = "My individuals")
) + ggplot2::scale_y_log10()

## ----load_advanced, echo=FALSE,warning=FALSE,message=FALSE---------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
work_dir <- file.path(theophylline, "Monolix")
input_data <- file.path(theophylline, "data_pk.csv")

ctr <- theophylline()

## ----get_input_data------------------------------------------------------
ctr %>% get_data("input")

## ----get_outputs_data----------------------------------------------------
ctr %>% get_data("estimates")
ctr %>% get_data("eta")
ctr %>% get_data("predictions")
ctr %>% get_data("finegrid")

## ----get_ipred_iwres, out.width='.9\\linewidth', fig.height=3, fig.width=3, fig.show='hold', fig.align='center'----
ctr %>% get_plot("dv_pred")
ctr %>% get_plot("eta_box")

## ----add_smooth, out.width='.49\\linewidth', fig.width=3, fig.height=3, fig.show='hold', fig.align='center'----
p1 <- ctr %>% get_plot("npde_time")
p2 <- p1 + geom_smooth(color = "red", linetype = "dashed")
p2

## ----ind_plots-----------------------------------------------------------
ctr %>% get_plot("individual", npage = 2)

## ----new_res_plot, fig.height=3, fig.width=7, fig.align='center'---------
p1 <- ctr %>%
  set_plot(
    "SCATTER", pname = "pred_vs_time", x = "TIME", y = "PRED", strat.facet = "STUD",
    labels = list(title = "Individual predictions and data per study")
  ) %>%
  get_plot("pred_vs_time")
p2 <- p1 + geom_line(aes(group = as.factor(ID), color = as.factor(ID)))
p2

## ----new_dis_plot, fig.height=3, fig.width=3, fig.align='center'---------
ctr %>%
  set_plot("DIS", pname = "distr1", type = "box", is.jitter = FALSE,is.shrink=TRUE) %>%
  get_plot("distr1")

## ----new_ind_plot, fig.height=3, fig.width=3, fig.align='center'---------
ctr %>%
  set_plot("IND", pname = "ind1") %>%
  get_plot("ind1", 4) # display only the fourth page

## ----indiv_plots_update--------------------------------------------------
ctr %>% pmx_update(
  "individual",
  labels = list(title = "My individuals")
)
ctr %>% get_plot("individual", npage = 1)

## ----update_pmx_plot_individual1-----------------------------------------
ctr %>% pmx_plot_individual(labels = list(title = "My individuals"), npage = 1)

## ----indiv_plots_grid----------------------------------------------------
ctr %>% pmx_update(
  "individual",
  facets = list(nrow = 2, ncol = 4),
  point = list(shape = 4, color = "red"),
  labels = list(title = "My individual plots grid")
)
ctr %>% get_plot("individual", npage = 1)

## ----update_pmx_plot_individual2-----------------------------------------
ctr %>% 
  pmx_plot_individual(
   facets = list(nrow = 2, ncol = 4),
   point = list(shape = 4, color = "red"),
   labels = list(title = "My individual plots grid"),
  npage = 1)

## ----update_xy_labels----------------------------------------------------

# Default individual plot
p1 <- ctr %>% pmx_plot_individual

p2 <- ctr %>%
  pmx_plot_individual(
    labels=list(
      x="Time (days)" ,
      y="Free serum concentration (nmol)")
  )
p1
p2

## ----update_res_plot2,  out.width='.49\\linewidth', fig.width=3, fig.height=3, fig.show='hold',fig.align='left'----


p1 <- ctr %>% pmx_plot_dv_pred

p2 <- ctr %>%
  pmx_plot_dv_pred(
    labels = list(x = "Population predictions", y = "Observations"),
    is.hline = FALSE, 
    point = list(color = "green4",shape=5)
  ) 



# Add the identity line and a smooth through the data:
p3 <- p2 +
  geom_abline(intercept = 0, aes(colour = "red")) +
  geom_smooth(aes(color = "red"), show.legend = FALSE)
p1
p3

## ----update_res_plot,  out.width='.49\\linewidth', fig.width=4, fig.height=3,fig.show='hold',fig.align='center'----

# Default iwres_ipred plot

p1 <- ctr %>% pmx_plot_iwres_ipred

# Above lines give the same result as the following:
# p1a = ctr %>% get_plot("iwres_ipred")

# Add bands and update title
p2 <- ctr %>%
  pmx_plot_iwres_ipred(
    is.band = TRUE,
    labels = list(title = "IPRED versus IWRES (with bands)")
  ) 
p1
p2

## ----fig.width=5, fig.height=4-------------------------------------------
ctr %>%
  pmx_plot_iwres_ipred(
    draft = list(size = 20, color = "purple", label = "FINAL")
  ) 

## ----advanced_pmx_plot---------------------------------------------------
## create a new residual plot 

ctr <- theophylline()
ctr %>% set_plot(
  ptype = "SCATTER",
  pname = "pred_time",
  x ="TIME",y="PRED")
## get the plot using pmx_plot

ctr %>% pmx_plot("pred_time")


## ----adv_update_pmx_plot_pred_time---------------------------------------

ctr %>% pmx_plot(pname = "pred_time",
                           point = list(alpha=0.1),
                           smooth=list(method="lm",color="blue"))

## ----load_config_advanced, echo=FALSE,warning=FALSE,message=FALSE--------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)
library(yaml)

## ---- echo=FALSE---------------------------------------------------------
theophylline <- file.path(
  system.file(package = "ggPMX"), "testdata",
  "theophylline"
)
WORK_DIR <- file.path(theophylline, "Monolix")
custom_inputs_file=file.path( system.file(package = "ggPMX"),"examples/custom_inputs.yaml")
cat(as.yaml(yaml.load_file(custom_inputs_file)))

## ----adv_custom_configs_files,echo=FALSE---------------------------------

theophylline <- file.path(
  system.file(package = "ggPMX"), "testdata",
  "theophylline"
)
WORK_DIR <- file.path(theophylline, "Monolix")

custom_plots_file=file.path( system.file(package = "ggPMX"),"examples/plots.yaml")

cat(as.yaml(yaml.load_file(custom_plots_file)))

## ----adv_custom_configs--------------------------------------------------
# *************** Create a controller using custom plot configuration ***************** ------

theophylline <- file.path(
  system.file(package = "ggPMX"), "testdata",
  "theophylline"
)
WORK_DIR <- file.path(theophylline, "Monolix")

custom_plots_file=file.path( system.file(package = "ggPMX"),"examples/plots.yaml")
ctr <-  pmx_mlx(
  config = pmx_config(
    plots=custom_plots_file,
    inputs = custom_inputs_file
  ),
  directory = WORK_DIR,
  input = file.path(theophylline, "data_pk.csv"),
  dv = "Y",
  dvid = "DVID",
  cats = c("SEX"),
  conts = c("WT0", "AGE0"),
  strats = "STUD"
)

## get the list of plots
ctr %>% plots


## ----custom_plots,  out.width='.49\\linewidth', fig.width=3, fig.height=3, fig.show='hold',fig.align='left'----
ctr %>% pmx_plot("custom_res_time")
ctr %>% pmx_plot("custom_npde_time")

## ----load_strat, echo=FALSE,warning=FALSE,message=FALSE------------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

## ----create_controller_uc------------------------------------------------

uc.name <- "1_popPK_model"
data_file <- "PKdata_ggPMX.csv"

uc.dir <- file.path(
  system.file(package = "ggPMX"), "testdata",
  uc.name
)
wd.mlx <- file.path(uc.dir, "RESULTS")
input_file <- file.path(uc.dir, data_file)

ctr <- pmx_mlx(
  "standing",
  directory = wd.mlx,
  input = input_file,
  dv = "DV",
  dvid = "YTYPE",
  cats = c("SEX", "RACE", "DISE", "ILOW"),
  conts = c("AGE0", "WT0", "HT0", "TRT"),
  occ = "ISS"
)

## ------------------------------------------------------------------------

ctr %>% pmx_plot_iwres_ipred(strat.facet = SEX + ILOW~RACE + DISE)

## ----fig.width=4, fig.height=3-------------------------------------------

## simpler 
ctr %>% pmx_plot_iwres_ipred(strat.color = "AGE0")

## ------------------------------------------------------------------------

ctr %>% pmx_plot_iwres_ipred(strat.facet = SEX~RACE)

## ----fig.width=3, fig.height=5-------------------------------------------
ctr %>% pmx_plot_eta_box(strat.facet = ~SEX)

## ------------------------------------------------------------------------
ctr %>% pmx_plot_eta_box(
  strat.facet = SEX~RACE,
  strat.color="DISE",
  is.jitter = FALSE,
  shrink=list(hjust=0.7)
)

## ------------------------------------------------------------------------
ctr %>% pmx_plot_eta_hist(strat.facet = "SEX")

## ----load_shrink, echo=FALSE,warning=FALSE,message=FALSE-----------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
work_dir <- file.path(theophylline, "Monolix")
input_data <- file.path(theophylline, "data_pk.csv")

ctr <- theophylline()

## ----shrink_comp---------------------------------------------------------
ctr %>% pmx_comp_shrink

## ----shrink_plot_box-----------------------------------------------------
ctr %>% pmx_plot_eta_box


## ----shrink_plot_hist----------------------------------------------------
ctr %>% pmx_plot_eta_hist


## ----shrink_plot_no------------------------------------------------------
ctr %>%   pmx_plot_eta_box( is.shrink = FALSE) 


## ---- compute_var--------------------------------------------------------
ctr %>% pmx_comp_shrink(  fun = "var")

## ---- shrink_plot_var----------------------------------------------------
ctr %>% pmx_plot_eta_box( shrink=list(fun = "var"))

## ----shrink_comp_strat---------------------------------------------------
ctr %>% pmx_comp_shrink(strat.facet = ~SEX)

## ----shrink_comp_strat_color---------------------------------------------
ctr %>% pmx_comp_shrink(strat.color = "SEX")

## ----shrink_plot_strat, fig.width=9, fig.height=8------------------------
ctr %>% pmx_plot_eta_hist(is.shrink = TRUE, strat.facet = ~SEX,
                          facets=list(scales="free_y"))

## ----fig.width=9, fig.height=8-------------------------------------------
ctr %>% pmx_plot_eta_box(is.shrink = TRUE, strat.facet = "SEX",
                          facets=list(scales="free_y",ncol=2))

## ----load_filter, echo=FALSE,warning=FALSE,message=FALSE-----------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
work_dir <- file.path(theophylline, "Monolix")
input_data <- file.path(theophylline, "data_pk.csv")

ctr <- theophylline()

## ------------------------------------------------------------------------

## this will change controller 
ctr %>% pmx_filter(data_set = "prediction", ID == 5 & TIME <2)
ctr %>% get_data("prediction")

## ----local_filtering-----------------------------------------------------
ctr <- theophylline()

## we can use any expression involving the data
ctr %>% pmx_plot_dv_pred(filter = DV > mean(DV) & PRED < median(PRED))
## filter and stratify
ctr %>% pmx_plot_dv_pred(filter = SEX == 1, strat.facet = ~SEX)



## ----load_settings, echo=FALSE,warning=FALSE,message=FALSE---------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

theophylline <- file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
work_dir <- file.path(theophylline, "Monolix")
input_data <- file.path(theophylline, "data_pk.csv")

ctr <- theophylline()

## ----settings_example, fig.width=5, fig.height=4,eval=FALSE--------------
#  
#  ## set one or more settings
#  my_settings <- pmx_settings(
#    is.draft = FALSE,
#    use.abbrev=TRUE,
#    ...) ### set other settings parameters here
#  ctr <-
#    pmx_mlx(
#      ..., ## put here other pmx parametes
#      settings = my_settings
#    )

## ----settings_is_draft,fig.height=3, fig.width=3, fig.show='hold', fig.align='center'----

ctr <- theophylline(settings=pmx_settings(is.draft = FALSE))

ctr %>% pmx_plot_dv_pred
ctr %>% pmx_plot_eta_box



## ----settings_get_abbrev-------------------------------------------------
ctr %>% get_abbrev

## ----settings_set_abbrev-------------------------------------------------
ctr %>% set_abbrev(TIME="TIME after the first dose")

## ----settings_use.abbrev-------------------------------------------------
ctr <- theophylline(settings=pmx_settings(use.abbrev = TRUE))
ctr %>% set_abbrev(TIME="Custom TIME axis")
ctr %>% pmx_plot_npde_time



## ----settings_use.finegrid-----------------------------------------------
ctr <- theophylline()
ctr %>% pmx_plot_individual(use.finegrid =FALSE)



## ----settings_color_scales_local-----------------------------------------
ctr <- theophylline()
ctr %>% pmx_plot_npde_time(strat.color="STUD")+ 
      ggplot2::scale_color_manual(
        "Study",
        labels=c("Study 1","Study 2"),
        values=c("1"="lightyellow","2"="lightblue"))
    


## ----serrings_solor_scales-----------------------------------------------

ctr <- theophylline(
  settings=
    pmx_settings(
      color.scales=list(
        "Study",
        labels=c("Study 1","Study 2"),
        values=c("1"="lightyellow","2"="lightblue"))
    )
)

ctr %>% pmx_plot_npde_time(strat.color="STUD")

ctr  %>%  pmx_plot_eta_box(strat.color="STUD")

## ----settings_cat_labels-------------------------------------------------


ctr <- theophylline(
  settings=
    pmx_settings(
      cats.labels=list(
        SEX=c("0"="M","1"="F"),
        STUD=c("1"="Study 1","2"="Study 2")
      ),
      use.labels = TRUE
    )
)


ctr %>%   pmx_plot_npde_time(strat.facet=~SEX)
ctr  %>%  pmx_plot_eta_box(strat.facet =~SEX)


## ----load_annex, echo=FALSE,warning=FALSE,message=FALSE------------------
knitr::opts_chunk$set(out.width = "100%", warning = FALSE, message = FALSE)
library(ggPMX)
library(ggplot2)
library(xtable)
library(knitr)

## ----plots_list,echo=FALSE,results='asis'--------------------------------

out <- rbind(
  c("Scatter plot of NPDE vs population predictions", "SCATTER", "npde_pred"),
  c("Scatter plot of NPDE vs time", "SCATTER", "npde_time"),
  c("Scatter plot of IWRES vs time", "SCATTER", "iwres_time"),
  c("Scatter plot of observations vs population predictions", "SCATTER", "dv_pred"),
  c("Scatter plot of observations vs individual predictions", "SCATTER", "dv_ipred"),
  c("Scatter plot of absolute value of IWRES vs individual predictions", "SCATTER", "abs_iwres_ipred"),
  c("Scatter plot of IWRES vs individual predictions", "SCATTER", "iwres_ipred"),
  c("Plots of observations and model predictions per individual", "IND", "individual"),
  c("Histogram of EBE", "DIS", "eta_hist"),
  c("Boxplot of EBE", "DIS", "eta_box"),
  c("Distribution and quantile-quantile plot of IWRES", "QQ", "qq_iwres"),
  c("Distribution and correlation structure of RE (`ETA`)", "ETA_PAIRS", "eta_matrix"),
  c("Relationships between RE and categorical covariates", "ETA_COV", "eta_cats"),
  c("Relationships between RE and continuous covariates", "ETA_COV", "eta_conts"),
  c("Visual predictive check (VPC)", "VPC", "vpc")
)

colnames(out) <- c("Plot Name", "ggPMX type", "ggPMX name")
xt <- xtable(out, label = "tab:plots_list", caption = "List of all diagnostic plots")
print(xt, comment = F)

## ----functions_list,echo=FALSE,results='asis'----------------------------

out <- rbind(
  c("1", "pmx, or pmx_mlx", "Creates a controller"),
  c("2", "plot_names or plots", "Lists controller plots"),
  c("3", "get_data", "Lists controller data"),
  c("4", "get_plot", "Prints a plot"),
  c("5", "set_plot", "Creates a new plot"),
  c("6", "pmx_update", "Updates an existing plot"),
  c("7", "pmx_filter", "Filters globally the data of the current session"),
  c("8", "pmx_copy", "Returns a deep copy of the controller")
)

colnames(out) <- c(" ", "Function name", "Description")

xt <- xtable(out, label = "tab:func_list", caption = "List of all `ggPMX` functions")
print(xt, comment = F)

## ----pmx_gpar_args-------------------------------------------------------
args(pmx_gpar)


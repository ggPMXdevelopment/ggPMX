\donttest{
# NOTES ######################
# examples are availables for all residual plots:
# - pmx_plot_abs_iwres_ipred
# - pmx_plot_dv_ipred
# - pmx_plot_dv_pred
# - pmx_plot_iwres_ipred
# - pmx_plot_iwres_time
# - pmx_plot_npde_time


# basic use  ---------------------------------------

ctr <- theophylline()
p <- ctr %>% pmx_plot_dv_pred()
## p is a ggplot2 object you can add any layer here
p + ggplot2::theme_minimal()

# update graphical parameter  ----------------------

## update labels
ctr %>% pmx_plot_dv_pred(
  labels = list(title = "DV versus PRED new title")
)

## remove draft
ctr %>% pmx_plot_dv_pred(is.draft = FALSE)

## remove horizontal line
ctr %>% pmx_plot_dv_pred(is.hline = FALSE)

## custom point aes and static parameters
## we can customize any geom_point parameter
ctr %>% pmx_plot_dv_pred(
  point = list(aes(alpha = DV), color = "green", shape = 4)
)



# stratification  ----------------------------------

## continuous stratification
ctr %>% pmx_plot_dv_pred(strat.color = "WT0")
## categorical stratification
ctr %>% pmx_plot_dv_pred(strat.facet = "SEX")
## using formula notation
ctr %>% pmx_plot_dv_pred(strat.facet = STUD~SEX)

# subsetting  --------------------------------------

## we can use any expression involving the data
ctr %>% pmx_plot_dv_pred(filter = DV > mean(DV) & PRED < median(PRED))
## filter and stratify
ctr %>% pmx_plot_dv_pred(filter = SEX == 1, strat.facet = ~SEX)


# transformation  --------------------------------------

## apply a log transformation in y
ctr %>% pmx_plot_dv_pred(trans = "log10_y")
}

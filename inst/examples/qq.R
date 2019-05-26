
\donttest{
# *************** basic use ***************** ------

ctr <- theophylline()
ctr %>% pmx_plot_eta_qq
ctr %>% pmx_plot_npde_qq
ctr %>% pmx_plot_iwres_qq

# update graphical parameter  ----------------------

## add reference line
ctr %>% pmx_plot_npde_qq(reference_line=list(color="blue"))

## remove reference line
ctr %>% pmx_plot_eta_qq(reference_line=NULL)


# stratification  ----------------------------------

## categorical stratification color parameter
ctr %>% pmx_plot_iwres_qq(strat.facet="STUD",strat.color="SEX")
## categorical stratification facetting
ctr %>% pmx_plot_eta_qq(strat.facet = "SEX")

## do not use symmetric axis
ctr %>% pmx_plot_npde_qq(xmax=FALSE,reference_line=list())
}


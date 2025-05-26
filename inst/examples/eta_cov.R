

\donttest{
# basic use  ---------------------------------------

ctr <- theophylline()
ctr %>% pmx_plot_eta_cats
ctr %>% pmx_plot_eta_conts

# update graphical parameter  ----------------------

## plot selected covariates
ctr %>% pmx_plot_eta_cats(
  covariates = pmx_cov(values = list("SEX"))
  )


## update labels
ctr %>% pmx_plot_eta_cats(
  labels = list(title = "New eta cats title")
)

## remove draft
ctr %>% pmx_plot_eta_cats(is.draft = FALSE)

## change text color line
ctr %>% pmx_plot_eta_conts(
  correl=list(colour="magenta")
  )

## set covariates custom labels 

ctr %>% pmx_plot_eta_conts(
  covariates=pmx_cov(values=list("WT0","AGE0"),
                     labels=list("Weight","Age"))
)

## set effects and covaraites custom labels


ctr <- theophylline( settings = pmx_settings(
  effects=list( levels=c("ka", "V", "Cl"), 
                labels=c("Concentration","Volume","Clearance")
  )
)
)
ctr %>% pmx_plot_eta_conts(
  covariates=pmx_cov(values=list("WT0","AGE0"),
                     labels=list("Weight","Age"))
)

}




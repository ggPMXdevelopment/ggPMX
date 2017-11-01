
# *************** basic use ***************** ------

ctr <- theophylline()
## boxplot variation
p <- ctr %>% pmx_plot_ebe_box()
## histogram variation
p <- ctr %>% pmx_plot_ebe_hist()

# update graphical parameter  ----------------------

## add jitter
ctr %>% 
  pmx_plot_ebe_box(has.jitter = TRUE, jitter = list(alpha = 0.4, color = "red"))

## remove shrinkage
ctr %>% 
  pmx_plot_ebe_hist(has.shrink = FALSE)

## updat histogram graphical parameters
ctr %>% 
  pmx_plot_ebe_hist(
    histogram = list(
      color = NA, 
      position = "fill", 
      binwidth = 1 / 100)
    )



# stratification  ----------------------------------

## categorical stratification fill parameter
## ctr %>% pmx_plot_ebe_hist(strat.color = "SEX")
## categorical stratification facetting
ctr %>% pmx_plot_ebe_hist(strat.facet = "SEX")
## using formula categorical stratification facetting
ctr %>% pmx_plot_ebe_box(strat.facet = STUD~SEX,
                         shrink=list(hjust=0.5))

# subsetting  --------------------------------------

## select a set of random effect
ctr %>% pmx_plot_ebe_box(filter = EFFECT %in% c("ka", "Cl"))
## filter and stratify by facets
ctr %>% pmx_plot_ebe_box(
  filter = EFFECT %in% c("ka", "Cl"), strat.facet = "SEX"
)
ctr %>% pmx_plot_ebe_hist(
  filter = EFFECT %in% c("ka", "Cl"), strat.facet = "SEX"
)

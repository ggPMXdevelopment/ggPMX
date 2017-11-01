# basic use  ---------------------------------------

ctr <- theophylline()
ctr %>% pmx_plot_individual(npage = 1)
## multiple pages
ctr %>% pmx_plot_individual(npage = c(1, 3))
## change faceting
ctr %>% pmx_plot_individual(facets = list(nrow = 5, ncol = 5), npage = 2)


# update graphical parameter  ----------------------

## update labels
ctr %>% pmx_plot_individual(
  labels = list(title = "Custom individual plot")
)

## remove draft
ctr %>% pmx_plot_individual(is.draft = FALSE)

## Customize ipred_line with any geom_line parameter
ctr %>% pmx_plot_individual(
  pred_line = list(color = "red", linetype = 20, alpha = 0.5)
)

## Customize ipred_line with any geom_line parameter
ctr %>% pmx_plot_individual(
  ipred_line = list(size = 5)
)


## Customize any geom_point parameter
ctr %>% pmx_plot_individual(
  point = list(aes(alpha = DV), color = "green", shape = 4)
)



# # stratification  ----------------------------------
#
# ## continuous stratification
ctr %>% pmx_plot_individual(strat.color = "WT0")
## categorical stratification
ctr %>% pmx_plot_individual(strat.facet = "SEX", facets = list(nrow = 5, ncol = 5))
## using formula notation
ctr %>% pmx_plot_individual(strat.facet = STUD~SEX)
#
# # subsetting  --------------------------------------
#
# ## we can use any expression involving the data
# ## filter and stratify
ctr %>% pmx_plot_individual(
  filter = SEX == 1, strat.facet = ~SEX, 
  facets = list(nrow = 5, ncol = 5))

# # transformation  --------------------------------------
#
# ## apply a log transformation in y
ctr %>% pmx_plot_individual(trans = "log10_y")
# ## apply a custonm trsnformation to normalize axis between 0 and 1
normalize <-
  function(x) {
    (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
ctr %>% pmx_plot_individual(trans = "normalize_xy")

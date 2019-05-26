
\donttest{

# basic use  ---------------------------------------

ctr <- theophylline()
p <- ctr %>% pmx_plot_eta_matrix

# update graphical parameter  ----------------------

## update labels
ctr %>% pmx_plot_eta_matrix(
  labels = list(title = "Eta matrix new title")
)

## remove draft
ctr %>% pmx_plot_eta_matrix(is.draft = FALSE)

## change text color line
ctr %>% pmx_plot_eta_matrix(
  text_color="red",
  shrink=list(mapping=aes(color="magenta"))
  )

## custom point aes and static parameters
## we can customize any geom_point parameter
ctr %>% pmx_plot_eta_matrix(
  point = list(color = "blue", shape = 4)
)



# stratification  ----------------------------------

## IGNORE continuous stratification
ctr %>% pmx_plot_eta_matrix(strat.color = "WT0")
## IGNORE  categorical stratification
ctr %>% pmx_plot_eta_matrix(strat.facet = "SEX")

# subsetting  --------------------------------------

## we can use any expression involving the data
ctr %>% pmx_plot_eta_matrix(filter = EFFECT%in%  c("Cl","ka"))
}

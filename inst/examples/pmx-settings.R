\donttest{

library(ggPMX)
library(ggplot2)
ctr <- theophylline(
  settings=
    pmx_settings(
      color.scales=list(
        "Study",
        labels=c("Study 1","Study 2"),
        values=c("1"="lightyellow","2"="lightblue")),
      cats.labels=list(
        SEX=c("0"="M","1"="F"),
        STUD=c("1"="Study 1","2"="Study 2")
      ),
      use.abbrev=TRUE,
      is.draft=TRUE,
      use.labels=TRUE
    )
)



ctr %>% 
  pmx_plot_npde_time(strat.color="STUD",strat.facet=~SEX)
# 
# 
ctr  %>%
  pmx_plot_eta_box(strat.color="STUD", strat.facet =~SEX)

ctr  %>%  pmx_plot_eta_hist
}

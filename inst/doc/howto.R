## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,eval=FALSE)
library(ggPMX)
library(ggplot2)
ctr <- theophylline()

## ----res_basic-----------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred

## ----res_update----------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(point = list(color = "blue", shape = 2, size = 2))

## ----res_grid------------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred() +
#    theme(panel.grid = element_blank())
#  

## ----res_draft-----------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(is.draft=FALSE)
#  ctr %>% pmx_plot_dv_pred(is.draft=TRUE,draft=list(size=14,color="blue"))
#  

## ----res_xy--------------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(labels = list(x = 'Bla_x', y= 'Bla_y')) +
#    theme(
#      axis.title=element_text(size=20),
#      axis.text=element_text(size=15)
#    )

## ----res_identity--------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(is.identity_line=FALSE)
#  ctr %>% pmx_plot_dv_pred(is.identity_line=TRUE,
#                           identity_line=list(color="green"))
#  

## ----res_smoothing1------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(is.smooth=FALSE)

## ----res_smoothing2------------------------------------------------------
#  
#  ctr %>% pmx_plot_dv_pred(smooth=list(color='green3', size=2, linetype=5,fun="lm"))

## ----res_strat-----------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(strat.facet = ~SEX)

## ----res_strat2----------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(strat.color = "SEX")

## ----res_strat_options---------------------------------------------------
#  
#  ctr <- theophylline()
#  col_scale <-
#    list(
#      "SEX",
#      labels=c("M","F"),
#      values=c("1"="lightyellow","0"="lightblue")
#    )
#  
#  ctr %>% pmx_plot_dv_pred(
#    strat.color = "SEX",
#    color.scales= col_scale)
#  

## ----res_strat_conts-----------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(strat.color = "WT0")

## ----res_strat_font_facet------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(strat.facet = ~SEX) +
#    theme(strip.text = element_text(size=20))

## ----res_strat_label_facet-----------------------------------------------
#  ## set labels once
#  ctr <- theophylline(
#    settings=
#      pmx_settings(
#  
#        cats.labels=list(
#          SEX=c("0"="M","1"="F")
#        ),
#        use.labels=TRUE
#      )
#  )
#  ## Now any plot with stratification will use labels
#  ctr %>% pmx_plot_dv_pred(strat.facet = ~SEX)

## ----res_use.abbrev------------------------------------------------------
#  ctr <- theophylline(
#    settings=
#      pmx_settings(
#        use.abbrev = TRUE
#      )
#  )
#  ctr %>% pmx_plot_dv_pred

## ------------------------------------------------------------------------
#  
#  ctr %>% pmx_plot_dv_pred(strat.facet = ~WT0,facets=list(ncol=10))

## ----warning=FALSE-------------------------------------------------------
#   ctr %>% pmx_plot_dv_pred(ranges = list(x = c(0,200), y=c(0,300)))

## ----warning=FALSE-------------------------------------------------------
#  ctr %>% pmx_plot_dv_pred(scale_y_log10 = TRUE, scale_y_log10=TRUE)

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_individual(npage=1)

## ----fig.height=7, fig.width=6-------------------------------------------
#  ctr %>% pmx_plot_individual(npage=1, dname = 'predictions')

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_individual(npage=1, is.legend=TRUE)
#  

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_individual(npage=1, is.draft=FALSE)
#  

## ------------------------------------------------------------------------
#  ctr %>%
#    pmx_plot_individual(
#      point = list(aes(alpha = DV),
#                   color = "red",
#                   shape = 8,
#                   size = 4
#                   )
#      )

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_individual(pred_line = list(color = "red", linetype = 20, size = 3, alpha = 0.5))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_individual(ipred_line = list(color = "blue", linetype = 2, size = 2))

## ---- warning=FALSE------------------------------------------------------
#  ctr %>% pmx_plot_individual(ranges = list(x = c(0,10), y=c(0,500)))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_individual(labels = list(x = 'Bla_x', y= 'Bla_y'))

## ----fig.height=7, fig.width=6-------------------------------------------
#  ctr %>% pmx_plot_individual +
#    ggplot2::theme(axis.title=ggplot2::element_text(size=20))

## ----fig.height=7, fig.width=6-------------------------------------------
#  ctr %>% pmx_plot_individual +
#    ggplot2::theme(axis.text=ggplot2::element_text(size=20))

## ----fig.height=7, fig.width=6-------------------------------------------
#  ctr %>% pmx_plot_individual(facets = list(nrow = 5, ncol = 5), npage = 2)

## ----fig.height=7, fig.width=6,warning=FALSE-----------------------------
#  ctr %>% pmx_plot_individual(scale_x_log10 = TRUE, scale_y_log10=TRUE)

## ----fig.height=7, fig.width=6-------------------------------------------
#  
#  ctr %>% pmx_plot_individual(
#    filter = SEX == 1, strat.facet = ~SEX,
#    facets = list(nrow = 6, ncol = 6))
#  

## ------------------------------------------------------------------------
#   ctr %>% pmx_plot_eta_box

## ------------------------------------------------------------------------
#   ctr %>% pmx_plot_eta_hist

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_box(is.shrink=FALSE)
#  ctr %>% pmx_plot_eta_hist(is.shrink=FALSE)
#  

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_hist(filter=EFFECT %in% c("ka","Cl"))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_hist(is.draft=FALSE)

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_hist(histogram=list(fill="gray"))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_hist(histogram=list(fill='green',color='green'))
#  

## ---- warning=FALSE------------------------------------------------------
#  ctr %>% pmx_plot_eta_hist(ranges=list(x=c(-0.2,0.2)))
#  ctr %>% pmx_plot_eta_box(ranges=list(y=c(-0.1,0.2)))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_box(labels=list(x="X_LAB",y="YLAB"))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_hist +
#   ggplot2::theme(
#     axis.title=ggplot2::element_text(size=32),
#     axis.text=ggplot2::element_text(size=8))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_hist(strat.facet=~STUD)
#  ctr %>% pmx_plot_eta_hist(strat.color="STUD")
#  ctr %>% pmx_plot_eta_box(strat.color="STUD",shrink=list(annotation="sh"))
#  ctr %>% pmx_plot_eta_box(strat.facet=~STUD, shrink=list(annotation="sh"))
#  

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(is.draft = FALSE)

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(point = list(shape = 4))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(point = list(color = "blue", size=3))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(point = list(color = "blue", size=3))

## ----warning=FALSE-------------------------------------------------------
#  
#  ctr %>% pmx_plot_eta_matrix(ranges = list(x = c(-1,1), y=c(-1,1)))
#  
#  

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(
#    labels = list(title = "Eta on CL/F",
#                  x="Eta_X",
#                  y="Eta_Y"
#    )
#  )
#  

## ------------------------------------------------------------------------
#  p <- ctr %>% pmx_plot_eta_matrix(
#    labels = list(x = 'Bla_x', y= 'Bla_y'),
#    axis.text = element_text(size=20))
#  
#  p + theme(
#    axis.text=element_text(size=5),
#    axis.title = element_text(size = 20))
#  

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(is.hline=TRUE)

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(is.smooth=FALSE)

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_eta_matrix(is.smooth=TRUE,smooth=list(fun="lm",color="red"))

## ----npde_pred-----------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred( is.draft=FALSE)

## ----warning=FALSE-------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(ranges = list(x = c(0,220), y=c(0,1)))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(labels=list(x="X_LAB",y="Y_LAB"))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred() +
#   ggplot2::theme(axis.title=ggplot2::element_text(size=11))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred +
#  ggplot2::theme(axis.text=ggplot2::element_text(size=8))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(hline = list(color="green",linetype=2))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(smooth = list(color="blue",linetype=2))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(is.hline=FALSE, is.smooth = FALSE)

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(point=list(shape=4,color="yellow",size=3))

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(strat.color="SEX",strat.facet=~STUD)

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(strat.color="WT0")

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(
#    strat.color = "SEX",
#    color.scales= list(values=c("1"="lightyellow","0"="lightblue"))
#  )

## ------------------------------------------------------------------------
#  ctr %>% pmx_plot_npde_pred(
#    strat.facet = ~STUD,
#    facets=list(nrow=2)
#  )


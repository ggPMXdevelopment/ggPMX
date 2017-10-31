get_plot_defaults <- 
  function(pname){
    
    switch(pname,
           dv_pred = list(
             labels = list(
               title="DV vs PRED",
               subtitle = "",
               x = "PRED",
               y = "DV"
             ), 
             point = list(shape = 1, color = "black", size = 1), 
             has.smooth=TRUE,
             smooth=list(se=FALSE,color="red",linetype=1),
             has.identity_line=TRUE,
             identity_line=list(intercept=0,color="blue")
           ),
           dv_ipred = list(
             labels = list(
               title="DV vs IPRED",
               subtitle = "",
               x = "IPRED",
               y = "DV"
             ), 
             point = list(shape = 1, color = "black", size = 1), 
             has.smooth=TRUE,
             smooth=list(se=FALSE,color="red",linetype=1),
             has.identity_line=TRUE,
             identity_line=list(intercept=0,color="blue")
           ),
           iwres_ipred =
             list(
               labels = list(
                 title="IWRES vs IPRED",
                 subtitle = "",
                 x = "IWRES",
                 y = "Individual predictions"
               ), 
               point = list(shape = 1, color = "black", size = 1), 
               add_hline=TRUE, 
               has.smooth=TRUE,
               smooth=list(se=FALSE,color="red",linetype=1)
             ),
           abs_iwres_ipred = 
             list(
               labels = list(
                 title="|IWRES| vs IPRED",
                 subtitle = "",
                 y= "|IWRES|",
                 x = "Individual predictions"
               ), 
               point = list(shape = 1, color = "black", size = 1), 
               add_hline=TRUE, 
               has.smooth=TRUE,
               smooth=list(se=FALSE,color="red",linetype=1),
               trans = "abs_y"
             ),
           iwres_time = list(
             labels = list(
               title="IWRES vs TIME",
               subtitle = "",
               x = "TIME",
               y = "Individual predictions"
             ), 
             point = list(shape = 1, color = "black", size = 1), 
             add_hline=TRUE, 
             has.smooth=TRUE,
             smooth=list(se=FALSE,color="red",linetype=1)
           ),
           
           npde_time = list(
             labels = list(
               title="NPDE vs TIME",
               subtitle = "",
               x = "TIME",
               y = "NPDE"
             ), 
             point = list(shape = 1, color = "black", size = 1), 
             add_hline=TRUE, 
             has.band=TRUE, 
             has.smooth=TRUE,
             smooth=list(se=FALSE,color="red",linetype=2)
           ),
           
           npde_pred = list(
             labels = list(
               title="NPDE vs PRED",
               subtitle = "",
               x = "PRED",
               y = "NPDE"
             ), 
             point = list(shape = 1, color = "black", size = 1), 
             add_hline=TRUE, 
             has.band=TRUE, 
             has.smooth=TRUE,
             smooth=list(se=FALSE,color="red",linetype=2)
           ),
           eta_matrix =
             list(    
               title= "Correlations of random effects",
               dname="eta",
               type.eta="mode",
               text_color="black"),
           ebe_box =
             list(     
               has.jitter = FALSE,
               jitter = list(shape = 1, color = "grey50", width = 0.1,height=0.1),
               has.shrink = TRUE,
               shrink=list(fun="sd",size=4,color="black"),
               histogram=list(binwidth=1/30,position = "dodge")
             ),
           
           
           ebe_hist = 
             list(    
               facets = list(scales = "free_y", nrow = 3),
               has.shrink = TRUE,
               shrink=list(
                 fun="sd",size=4,color="black",hjust=-0.5,vjust=2)
             ),
           
           
           iwres_qq = 
             list(        
               labels = list(
                 title="",
                 subtitle = "",
                 x = "Standard Normal Quantiles",
                 y = "IWRES Quantiles"
               )
             ),
           
           npde_qq =
             list(
               labels = list(
                 title="",
                 subtitle = "",
                 x = "Standard Normal Quantiles",
                 y = "NPDE Quantiles"
               )
             ),
           list())
    
  }




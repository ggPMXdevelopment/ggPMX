# 
# 
# 
# plotVPC <- function(sumData, 
#                     obsData, 
#                     facetBy,
#                     ncol=NULL, 
#                     showObs=TRUE,
#                     showPI=FALSE, 
#                     showBands=FALSE,
#                     logY=FALSE, 
#                     showLegend=TRUE) {
#   
#   # Generates a VPC plot.
#   #
#   # Args:
#   #   sumData: data.frame containing the simulations.
#   #            Expects the following columns:
#   #            - vpcTime
#   #            - obsMedian: median of observed data
#   #            - Optional (if showPI set to TRUE): obsLower, obsUpper:
#   #              lower and upper limits of observed data range (min/max
#   #              or any suited quantile of the data)
#   #            - median: median of simulated data
#   #            - Optional (if showBands set to TRUE): lRangeOfMedian, uRangeOfMedian: 
#   #              range of simulated data median (e.g. 10% and 90% quantiles)
#   #            - Optional (if showPI set to TRUE): upper: 
#   #              upper limit of simulated data range (min/max or any 
#   #              suited quantile) 
#   #            - Optional (if options showPI and showBands set to TRUE): 
#   #                lRangeOfUpper, uRangeOfUpper: 
#   #              range of upper end of simulated data range 
#   #              (e.g. 10% and 90% quantiles)
#   #            - Optional (if options showPI and showBands set to TRUE): 
#   #                lRangeOfLower, uRangeOfLower: 
#   #              range of lower end of simulated data range 
#   #              (e.g. 10% and 90% quantiles)
#   #   obsData: data.frame containing the observations.
#   #            Expects the following columns:
#   #            - vpcTime
#   #            - LIDV
#   #            - Column for faceting as specified by facetBy
#   #   facetBy: The name of a column you want to use for faceting
#   #   ncol: Number of columns when faceting
#   #   
#   #   showObs: If TRUE, plot observations
#   #   showPI: If TRUE, plot prediction intervals and corresponding data summary
#   #   showBands: If TRUE, plot shaded bands around the simulations
#   #   logY: If TRUE, plot y axis on log scale
#   #   showLegend: If TRUE, generate the plot with a legend
#   #
#   # Returns:
#   #   A ggplot object.
#   
#   vpcPlot <- ggplot(data=sumData,aes(x=vpcTime))
#   
#   # Now, build up the plot layer by layer: start with the geom_ribbons
#   # to make sure the lines are not partly hidden below them
#   
#   # Plot the ranges of the uper and lower boundaries of the simulated studies
#   if (showPI & showBands) {
#     vpcPlot <- vpcPlot + 
#       geom_ribbon(aes(ymin=lRangeOfUpper, ymax=uRangeOfUpper, 
#                       fill="minmax", alpha="minmax")) + 
#       geom_ribbon(aes( ymin=lRangeOfLower, ymax=uRangeOfLower, 
#                        fill="minmax", alpha="minmax")) 
#   }
#   
#   # Plot the ranges of the medians of the simulated studies
#   if (showBands) {
#     vpcPlot <- vpcPlot + 
#       geom_ribbon(data=sumData, 
#                   aes(x=vpcTime, ymin=lRangeOfMedian, ymax=uRangeOfMedian, 
#                       fill="median", alpha="median"))
#   }
#   
#   # Add the individual observations
#   if (showObs) {
#     vpcPlot <- vpcPlot + geom_point(data=obsData, 
#                                     aes(x=vpcTime, y=LIDV, 
#                                         colour="data", shape="data"))
#   }
#   
#   # Plot the medians of the upper and lower boundaries
#   if (showPI) {
#     
#     # Different implementations required for option showBands on/off:
#     # To make sure the legend is displayed properly, either the bands or the 
#     # lines (but not both) must have alpha and fill defined
#     
#     if (showBands) {
#       vpcPlot <-  vpcPlot + 
#         geom_line(data=sumData, 
#                   aes(x=vpcTime, y=upper, 
#                       colour="minmax", linetype="minmax", shape="minmax")) + 
#         geom_line(data=sumData, 
#                   aes(x=vpcTime, y=lower, 
#                       colour="minmax", linetype="minmax", shape="minmax")) 
#     } else {
#       vpcPlot <-  vpcPlot + 
#         geom_line(data=sumData, 
#                   aes(x=vpcTime, y=upper, 
#                       colour="minmax", linetype="minmax", shape="minmax", 
#                       fill="minmax", alpha="minmax")) + 
#         geom_line(data=sumData, 
#                   aes(x=vpcTime, y=lower, 
#                       colour="minmax", linetype="minmax", shape="minmax", 
#                       fill="minmax", alpha="minmax")) 
#     }
#   }
#   
#   # Plot the upper and lower range lines of the observed data
#   if (showPI) {
#     vpcPlot <- vpcPlot + 
#       geom_line(data=sumData, 
#                 aes(x=vpcTime, y=obsLower, 
#                     colour="data", linetype="minmax", alpha="data")) + 
#       geom_line(data=sumData, 
#                 aes(x=vpcTime, y=obsUpper, 
#                     colour="data", linetype="minmax", alpha="data")) 
#   } 
#   
#   # Plot the medians of the median 
#   if (showBands) {
#     
#     # Different implementations required for option showBands on/off:
#     # To make sure the legend is displayed properly, either the band or the 
#     # line (but not both) must have alpha and fill defined
#     
#     vpcPlot <-  vpcPlot + 
#       geom_line(data=sumData, 
#                 aes(x=vpcTime, y=median, 
#                     colour="median", linetype="median", shape="median"))
#   } else {
#     vpcPlot <-  vpcPlot + 
#       geom_line(data=sumData, 
#                 aes(x=vpcTime, y=median, 
#                     colour="median", linetype="median", shape="median", 
#                     fill="median", alpha="median"))
#   }
#   
#   # Plot median of the observed data
#   vpcPlot <- vpcPlot + 
#     geom_line(data=sumData, 
#               aes(x=vpcTime, y=obsMedian, 
#                   colour="data", linetype="data", alpha="data", fill="data"))
#   
#   
#   # Set log scale on y axis
#   if(logY) vpcPlot <- vpcPlot + scale_y_log10()
#   
#   # Faceting
#   vpcPlot <- vpcPlot + facet_wrap(as.formula(paste("~", facetBy)), ncol=ncol) 
#   
#   # Set legend title & entries
#   legTitle <- ""
#   leg <- c("Median",  
#            "Prediction interval", 
#            "Observed")
#   if (showBands) {
#     leg <- 
#       paste0(leg,c("\n(median and range)", "\n(median and range)", ""))
#   } else {
#     leg <- 
#       paste0(leg,c(" (median)", "\n(median)", ""))
#   }
#   
#   # Set the object styles
#   lLevels <- c("median" = "solid", "minmax" = "dashed", "data" = "solid")
#   sLevels <- c("median" = 32, "minmax" = 32, "data" = 20)
#   fLevels <- c("median" = "pink", "minmax" = "lightblue", "data" = "white")
#   cLevels <- c("median" = "red", "minmax" = "blue", "data" = "black")
#   aLevels <- c("median" = 1, "minmax" = 1, "data" = 1)
#   # Exception for alphas, if PI should should be shown
#   if (showBands) c("median" = 0.4, "minmax" = 0.4, "data" = 1)
#   # Set order of items in the legend:
#   breaks <- c("median", "minmax", "data")
#   
#   # Apply styles and legends
#   vpcPlot <- vpcPlot +  
#     scale_linetype_manual(legTitle, values=lLevels, breaks=breaks, labels=leg) +  
#     scale_shape_manual(legTitle, values=sLevels, breaks=breaks, labels=leg) +  
#     scale_fill_manual(legTitle, values=fLevels, breaks=breaks, labels=leg) +  
#     scale_colour_manual(legTitle, values=cLevels, breaks=breaks, labels=leg) +  
#     scale_alpha_manual(legTitle, values=aLevels, breaks=breaks, labels=leg)
#   
#   # Switch legends off
#   if (!showLegend) vpcPlot <- vpcPlot + theme(legend.position = "none") 
#   
#   # Add the axis titles
#   vpcPlot <- vpcPlot + xlab("Time after first dose (h)")
#   vpcPlot <- vpcPlot + ylab("Plasma concentration (ng/mL)")
#   
#   return(vpcPlot)
# }
# 
# 


#' Prepare nm vpc data
#'
#' @param data_file input data file
#' @param nm_dir simulation dir
#' @param nm_sim_file dimulation file
#' @param irun simulation run number ( nonmem)
#' @param key merge column
#' @param group_by summary key
#'
#' @return data.table
#' @importFrom stats quantile median as.formula

vpc.data <- 
  function(
    data_file = "~/Downloads/DataManipulationWithR_HandsOn/data/pkDatExample.csv",
    nm_dir = "~/Downloads/DataManipulationWithR_HandsOn/nonmem/",
    nm_sim_file = file.path(nm_dir,sprintf("run%isim1.nmtab",irun)),
    irun = 99,
    key = "RID",
    group_by = c("VIS1N", "NT", "NDOSE", "PART", "meanTIM2")
  ){
    
    sim <- fread(nm_sim_file, skip=1,fill = TRUE)
    dat <- fread(data_file)
    CMT <- DV  <- EFFECT  <- EVID  <- NULL
    ID  <- ISIM  <- LIDV  <- NT <- NULL
    PART <-  POS <-  TIM2 <-  VALUE <-  VIS1N <- NULL
    q10 <- q90 <- meanTIM2 <- med_dv <- NULL
    
    ## format simulation file
    sim <- sim[ ,ISIM :=cumsum(ID == "ID") + 1][
      !ID%in%c("ID","TABLE")][
        ,ID:=NULL][,lapply(.SD,as.double)][,(key):=as.integer(get(key))]
    
    # Merge simulation with additional information from input data set
    sim2 <- merge(sim, dat[CMT==2 & EVID==0][
      ,meanTIM2 := mean(TIM2/24),list(VIS1N, NT ,PART)], by=key) 
    
    
    
    sim2[ISIM == 1,
         c("obsMedian","obsQ10","obsQ90"):=
           list(median(LIDV),
                quantile(LIDV, 0.1),
                quantile(LIDV, 0.9)),
         group_by]
    
    sim2[,c("med_dv","q10","q90"):=
           list(    median(DV),
                    quantile(DV, 0.1),
                    quantile(DV, 0.9)
           ),c(group_by, "ISIM")]
    
    
    sim2[,c("medianOfMedian","medianOfQ10","medianOfQ90","q10OfMedian",
            "q10OfQ10","q10OfQ90","q90OfMedian","q90OfQ10","q90OfQ90") :=
           list(median(med_dv),quantile(med_dv, 0.1),quantile(med_dv, 0.9),
             median(q10),quantile(q10, 0.1),quantile(q10, 0.9),
             median(q90),quantile(q90, 0.1),quantile(q90, 0.9)
           ),group_by]
    
    
    setnames(sim2,
             c("medianOfMedian","q10OfMedian", "q90OfMedian",
               "medianOfQ10", "q10OfQ10", "q90OfQ10",
               "medianOfQ90",  "q10OfQ90", "q90OfQ90",
               "obsQ10", "obsQ90"),
             c("median","lRangeOfMedian" ,
               "uRangeOfMedian","lower",
               "lRangeOfLower" ,"uRangeOfLower" ,
               "upper" , "lRangeOfUpper" ,"uRangeOfUpper" ,
               "obsLower","obsUpper") )
    
    
    sim2[PART == 1]
    
  }





vpc <- function(
  labels, 
  facets = list(by="NDOSE",ncol = 2), 
  dname = NULL,
  plot.pi=TRUE, 
  plot.bands=TRUE, 
  plot.obs=TRUE,
  has.legend=FALSE,
  ...){
  assert_that(is_list(facets))
  assert_that(is_string_or_null(dname))
  if(missing(labels))
    labels <- list(
      title = "VPC",
      subtitle = "",
      x = "Time after first dose (h)",
      y = "Plasma concentration (ng/mL)")
  assert_that(is_list(labels))
  if(is.null(dname)) dname <- "IND"
  
  # Set legend title & entries
  legTitle <- ""
  leg <- c("Median",  
           "Prediction interval", 
           "Observed")
  if (plot.bands) {
    leg <- 
      paste0(leg,c("\n(median and range)", "\n(median and range)", ""))
  } else {
    leg <- 
      paste0(leg,c(" (median)", "\n(median)", ""))
  }
  
  
  structure(list(
    ptype="VPC",
    dname=dname,
    labels = labels,
    facets = facets,
    plot.pi=plot.pi, 
    plot.bands=plot.bands, 
    plot.obs=plot.obs,
    has.legend=has.legend,
    gp = pmx_gpar(labels = labels,  ...)), 
    class = c("vpc", "pmx_gpar"))
}




plot_pmx.vpc <- function(x,dx,...) {
  
  
  # Select data to be included in the plot
  ISIM <- meanTIM2 <- vpcTime <- NULL
  sumData <- dx[,vpcTime := meanTIM2 / 24]  # Simulated and observed data summaries
  obsData <- dx[ISIM == 1][,vpcTime := meanTIM2 / 24] # Observed patient data
  
  
  p <- ggplot(data=sumData,aes_string(x="vpcTime"))
  with(x,{
    # Plot the ranges of the uper and lower boundaries of the simulated studies
    if (plot.pi && plot.bands)
      p <- p + 
        geom_ribbon(aes_string(ymin="lRangeOfUpper", ymax="uRangeOfUpper", 
                               fill="minmax", alpha="minmax")) + 
        geom_ribbon(aes_string( ymin="lRangeOfLower", ymax="uRangeOfLower", 
                                fill="minmax", alpha="minmax")) 
    
    if (plot.bands) 
      p <- p + 
        geom_ribbon(aes_string(ymin="lRangeOfMedian", ymax="uRangeOfMedian", fill="median", alpha="median"))
    if (add.obs) 
      p <- p + 
        geom_point(data=obsData, aes_string(y="LIDV",colour="data", shape="data"))
    
    # Plot the medians of the upper and lower boundaries
    if (plot.pi) {
      if (plot.bands) {
        p <-  p + 
          geom_line(aes_string( y="upper",colour="minmax", linetype="minmax", shape="minmax")) + 
          geom_line(aes_string(y="lower",colour="minmax", linetype="minmax", shape="minmax")) 
      } else {
        p <-  p + 
          geom_line(aes_string( y="upper",colour="minmax", linetype="minmax", shape="minmax", 
                                fill="minmax", alpha="minmax")) + 
          geom_line(aes_string(y="lower", colour="minmax", linetype="minmax", shape="minmax", 
                               fill="minmax", alpha="minmax")) 
      }
      p <- p + geom_line(data=obsData, 
                         aes_string( y="obsLower", colour="data", linetype="minmax", alpha="data")) + 
        geom_line(data=obsData, 
                  aes_string( y="obsUpper", colour="data", linetype="minmax", alpha="data")) 
      
    }
    
    if (plot.bands) { 
      p <-  p + geom_line(aes_string(y="median",colour="median", linetype="median", shape="median"))
    } else {
      p <-  p + geom_line(aes_string(y="median", colour="median", linetype="median", shape="median", 
                                     fill="median", alpha="median"))
    }
    p <-  p + geom_line(data=obsData, 
                        aes_string(y="median", colour="median", linetype="median", shape="median", 
                                   fill="median", alpha="median"))
    p <- p +  with(facets,facet_wrap(as.formula(paste("~", by)), ncol=ncol, scales="free") )
    p <- plot_pmx(gp, p)
  })
  
  
  p
}




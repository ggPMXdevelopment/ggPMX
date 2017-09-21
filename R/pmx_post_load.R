#' Merge input and fingrid data sets
#'
#' @param input \code{data.table} input data set
#' @param finegrid \code{data.table} finegrid data set
#' @param covariates \code{character} covariates names vector (optional)
#' @return data.table
#' @importFrom zoo na.locf

input_finegrid <- function(input, finegrid, covariates = NULL)
{
  ## this for R CMD check purpose
  ID <- TIME <- DVID <- NULL
  if(is.null(finegrid))return(NULL)
  dx <- rbind(finegrid, input, fill = TRUE)[TIME>0]
  measures <- c("DV","PRED","IPRED")
  if(!is.null(covariates) && all(nzchar(covariates)))
    measures <- c(measures,covariates)
  dx[order(ID,DVID,TIME)][,(measures):=
                            lapply(.SD, na.locf,na.rm=FALSE), by="ID,DVID",.SDcols = measures]
}


post_load_eta <- function(ds,input,sys){
  ID <- DVID <- VARIABLE <- NULL
  ## add DVID variable : merge key with the input 
  if(grepl("#",ds[1,ID],fixed=TRUE))
    ds[,c("ID","DVID") := tstrsplit(ID,"#")][, 
                                             c("ID","DVID"):=list(as.integer(ID),as.integer(DVID))]
  if(!"DVID" %in% names(ds))  ds[,DVID:=1]
  ds <- try(
    merge(ds, input, 
          by = c("ID", "DVID"))
    ,silent=TRUE)
  
  if(inherits(ds,"try-error"))
    stop("error cannot merge eta data with the modelling input")
  ## put in the long format 
  measures <- grep("_.*_", names(ds))
  ds[,(measures) := lapply(.SD,as.numeric),.SDcols =measures]
  ds <- melt(ds, measure = measures)
  setnames(ds, toupper(names(ds)))
  ## keep only mean or mode variable
  ds[grep("(mode|mean)$",VARIABLE)]
  ## reshape columns for easier filtering
  ds[, c("VAR", "EFFECT", "FUN") := 
       list(gsub("_.*","",VARIABLE),
            gsub("eta_(.*)_(mode|mean)","\\1",VARIABLE),
            gsub(".*_","",VARIABLE))]
  ds
}

post_load <- function(dxs, input, sys, dplot,...){
  ## avoid RCMDCHECK
  DVID <- ID <- NULL
  ## merge finegrid with input data 
  if(sys == "mlx"){
    dxs[["predictions"]]  <- merge(dxs[["predictions"]], input,by = c("ID", "TIME","DVID"))
    if(!is.null(dxs[["finegrid"]])){
      dxs[["finegrid"]] <- input_finegrid(input,dxs[["finegrid"]],...)
      dxs[["IND"]] <-  dxs[["finegrid"]] 
    }else{
      message("No finegrid file: we use instead predictions.txt for individual plots")
      dxs[["IND"]] <- dxs[["predictions"]] 
    }  
    
    ## prepare data set for stratification
    if(!is.null(dxs[["eta"]]))
      dxs[["eta"]] <- post_load_eta(dxs[["eta"]],input,sys)
    

  }
  
  dxs
}




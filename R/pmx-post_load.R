#' Merge input and fingrid data sets
#'
#' @param input \code{data.table} input data set
#' @param finegrid \code{data.table} finegrid data set
#' @return data.table
#' @importFrom zoo na.locf

input_finegrid <- function(input, finegrid)
{
  ## this for R CMD check purpose
  ID <- TIME <- DVID <- NULL
  if(is.null(finegrid))return(NULL)
  input[,source:="in"]
  dx <- rbind(finegrid,input,fill=TRUE)[order(DVID,ID,TIME)]
  
  measures <- setdiff(names(input),c("ID","DVID","DV","TIME","source"))
  if(length(measures)>0)
    dx[,(measures):=
         lapply(.SD, na.locf,na.rm=FALSE), by="ID,DVID",.SDcols = measures]
  input[,source:=NULL]
  dx[is.na(source) & TIME>=0][,source:=NULL]
}


post_load_eta <- function(ds,input,sys,occ){
  ID <- DVID <- VARIABLE <- NULL
  ## add DVID variable : merge key with the input 
  if(grepl("#",ds[1,ID],fixed=TRUE))
    ds[,c("ID","DVID") := tstrsplit(ID,"#")][,
        c("ID","DVID") :=list(as.integer(ID),as.integer(DVID))]
  if(!"DVID" %in% names(ds))  ds[,DVID:=1]
  
  keys <- c("ID", "DVID")
  if(occ!="") keys <- c(keys,if(length(occ)==1)"OCC" else sprintf("OCC%s",seq_along(occ)))
  ds <- try(
    merge(ds, input, 
          by = keys)
    ,silent=TRUE)
  
  if(inherits(ds,"try-error"))
    stop("error cannot merge eta data with the modelling input")
  if(nrow(ds)==0)
    stop("error cannot merge eta data with the modelling input: no individual match")
  ## put in the long format 
  measures <- grep("_.*_", names(ds))
  if(length(measures)==0){
    message("NO random effect found")
    return(ds)
  }
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

post_load <- function(dxs, input, sys, dplot,occ){
  ## avoid RCMDCHECK
  DVID <- ID <- NULL
  if(is.null(dxs[["predictions"]]))return(dxs)
  ## merge finegrid with input data 
  if(sys == "mlx"){
    keys <- c("ID", "TIME","DVID")
    if(occ!="") keys <- c(keys,if(length(occ)==1)"OCC" else sprintf("OCC%s",seq_along(occ)))
    
    dxs[["predictions"]]  <- 
      merge(dxs[["predictions"]], input, by=keys)
    if(!is.null(dxs[["finegrid"]])){
      dxs[["finegrid"]] <- input_finegrid(input,dxs[["finegrid"]])
      dxs[["IND"]] <-  dxs[["finegrid"]] 
    }else{
      message("
        NO FINEGRID FILE: 
        we will use instead predictions.txt for individual plots")
      dxs[["IND"]] <- dxs[["predictions"]] 
    }  
    
   
    
  }
  
  dxs
}




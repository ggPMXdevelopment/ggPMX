#' Read MONOLIX individual parameters
#' @param path character path to the file
#' @param x dataset object
#' @param ... extra paramter not used 
#'
#' @return data.table object
#' @import data.table

#' @export
read_mlx_ind_est <- function(path, x,...){
  ds <- pmx_fread(path)
  nn <- grep("^id|^eta_.*_(mode|mean)$",names(ds),
             ignore.case = TRUE,value=TRUE)
  ds <- ds[,nn,with=FALSE]
  setnames(ds,grep("^id$",names(ds),ignore.case = TRUE,value=TRUE),"ID")
  ## remove all null variables 
  ## NO RANDOM EFFECT
  ## TODO : treat case where we have epsilon as random effect
  valid_cols <- 
    c("ID",
      ds[, setdiff(names(ds), c("ID")), with = FALSE][,names(.SD)[colSums(.SD)!=0]]
    )
  ds <- ds[,valid_cols,with=FALSE]
  ## remove hash
  if(grepl("#",ds[1,ID],fixed=TRUE))
    ds[,c("ID","DVID") := tstrsplit(ID,"#")
       ][,
         c("ID","DVID"):=list(as.integer(ID),as.integer(DVID))]
  
  ds
}


#' Read Modelling input data 
#'
#' @param ipath full path of the input file
#' @param dv \code{character} the name of measurable variable used in the input modelling file
#' @param dvid \code{character} observation type parameter
#' @param cats \emph{[Optional]}\code{character} vector of categorical covariates
#' @param conts \emph{[Optional]}\code{character} vector of continuous covariates
#' @param strats \emph{[Optional]}\code{character} extra stratification variables

#' 
#' @return data.table well formatted containing modelling input data
#'
read_input <- function(ipath, dv,dvid, cats = "",conts="",strats=""){
  
  DVID <- TIME <- EVID <- MDV <- NULL 
  xx <- pmx_fread(ipath)
  
  setnames(xx, grep("^id$",names(xx),ignore.case = TRUE,value=TRUE), "ID")
  
  if(dv %in% names(xx)) setnames(xx, dv, "DV")
  else {
    dv.names <- paste(setdiff(names(xx),c("ID","id","time","TIME")),collapse=" or ")
    dv.names <- sprintf("'%s'",dv.names)
    err.msg <- sprintf("%s : is not a valid measurable variable
                        suggested names are : %s",dv,dv.names)
    stop(err.msg)
  }
  if(dvid %in% names(xx)) setnames(xx, dvid, "DVID")
  else xx[,DVID:=1]
  ## round time column for further merge
  setnames(xx, grep("^time$",names(xx),ignore.case = TRUE,value=TRUE), "TIME")
  xx[,TIME:=round(TIME,4)]
  
  setnames(xx,toupper(names(xx)))
  
  covariates <- unique(c(cats,conts))
  if(length(covariates[covariates!=""])){
    covariates <- covariates[covariates!=""]
    if(any(!covariates %in% names(xx))) 
      stop(sprintf("%s : is not a valid covariate variable\n",
                   covariates[!covariates%in%names(xx)]))
  }
  if(length(cats[cats!=""])>0){
    cats <- cats[cats!=""]
    xx[,(cats) := lapply(.SD,as.factor),.SDcols=cats]
  }
  if(length(strats[strats!=""])>0){
    strats <- strats[strats!=""]
    xx[,(strats) := lapply(.SD,as.factor),.SDcols=strats]
  }
  if(length(conts[conts!=""])>0){
    conts <- conts[conts!=""]
    xx[,(conts) := lapply(.SD,as.numeric),.SDcols=conts]
  }
  if(all(c("MDV","EVID") %in% names(xx)))
    xx[!(EVID==1 & MDV==1)]
  else   xx
  
}

#' Read MONOLIX model predictions
#'
#' @param path character path to the file
#' @param x dataset object
#' @param ... extra paramter not used
#'
#' @return data.table object
#' @import data.table

#' @export
read_mlx_pred <- function(path, x,...){
  
  ID <- DVID <- NULL
  xx <- pmx_fread(path)
  setnames(xx, tolower(names(xx)))
  if(!is.null(x$strict)) xx <- xx[, names(x$names), with = FALSE]
  ## use configuration columns
  ids <-which(names(x$names) %in% names(xx))
  nn <- x$names[ids]
  if(!"indpred_mode" %in% names(nn)){
    if("indpred_mean" %in% names(xx)){
      message("predictions: NO indpred_mode found use inpred_mean instead")
      nn[["indpred_mode"]] <- NULL 
      nn[["indpred_mean"]] <- "IPRED"
    }else{
      message("predictions: NO indpred_mode neither indpred_mean found")
      return(NULL)
    }
  }
  
  
  ## select columns
  res <- setnames(xx[,names(nn),with=FALSE],as.character(nn))
  
  if(grepl("#",res[1,ID],fixed=TRUE))
    res[,c("ID","DVID") := tstrsplit(ID,"#")][,c("ID","DVID"):=list(as.integer(ID),as.integer(DVID))]
  
  dvid <- as.list(match.call(expand.dots = TRUE))[-1]$dvid
  if(is.null(dvid) || !dvid %in% names(res)) res[,"DVID" :=1]
  else setnames(res,dvid,"DVID")
  
  res
}


#' Read MONOLIX parameter estimation file
#'
#' @param path character path to the file
#' @param x dataset object
#' @param ... extra parameter not used 
#'
#' @return data.table object
#' @importFrom utils read.table
#' @import data.table
#' @export
read_mlx_par_est <- function(path, x,...){
  xx <- setDT(read.table(path, sep = ";", header = TRUE))
  if("names" %in% names(x))
    setnames(xx, x[["names"]])
  xx
}

#' Load data set
#'
#' @param x data set config
#' @param path character path to the directory
#' @param sys \code{character} mlx or nm
#' @param ... extra parameter passed to special readers 
#'
#' @return data.table
#' @import data.table
#' @export
load_data_set <- function(x, path, sys,...){
  fpath <- file.path(path, x[["file"]])
  if(!file.exists(fpath)){
    fpath <- grep(x[["file"]], list.files(path, full.names = TRUE), 
                  value = TRUE)
    if(length(fpath) > 1)
      fpath <- grep(sys, fpath, ignore.case = TRUE, value=TRUE)
  }
  if(length(fpath)==0  || !file.exists(fpath)){
    message(sprintf(" %s FILE DOES NOT exist under %s", x[["file"]], path))
    return(NULL)
  }
  
  
  if(exists("reader", x))
    return(do.call(x[["reader"]], list(fpath, x,...)))
  ds <- pmx_fread(fpath)
  ds <- ds[,!grep("^V[0-9]+", names(ds)), with = FALSE]
  data.table::setnames(ds, tolower(names(ds)))
  if("names" %in% names(x)){
    setnames(ds,
             tolower(names(x[["names"]])),
             as.character(x[["names"]]))
    ds <- ds[, as.character(x[["names"]]), with = FALSE]
  }
  ds
}


#' Load all/or some source data set
#'
#' @param sys type cane mlx/nom
#' @param path \code{character} directory path containing all sources.
#' @param dconf configuration object
#' @param ... any extra parameters for readers
#'
#' @return list of data.table
#' @export
load_source <- function(sys, path, dconf,...){
  names. <- names(dconf)
  
  
  datasets <- dconf[names.]
  dxs <- lapply(datasets,function(x){
    load_data_set(x,  path = path,  sys = sys,...)
  })
  dxs
}

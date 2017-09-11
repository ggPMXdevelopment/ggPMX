#' Read MONOLIX individual parameters
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object
#' @import data.table

#' @export
read_mlx_ind_est <- function(path, x){
  ds <- pmx_fread(path)
  ds <- ds[,grep("id|eta",tolower(names(ds))),with=FALSE]
  ds <- ds[,as.logical(!grepl("*",tolower(names(ds)),fixed=TRUE)),with=FALSE]
  setnames(ds,grep("^id$",names(ds),ignore.case = TRUE,value=TRUE),"ID")
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
  
  DVID <- TIME <- NULL
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
  ## keep only potential used columns
  cnames <- unique(c("ID","DV","DVID","TIME",cats,conts,strats))
  cnames <- cnames[cnames!=""]
  
  xx[,cnames,with=FALSE]
  
}

#' Read MONOLIX model predictions
#'
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object
#' @import data.table

#' @export
read_mlx_pred <- function(path, x){
  
  ID <- DVID <- NULL
  xx <- pmx_fread(path)
  setnames(xx, tolower(names(xx)))
  if(!is.null(x$strict)) xx <- xx[, names(x$names), with = FALSE]
  ## mean start columns
  col_stars <- grep("*", names(xx), fixed = TRUE, value = TRUE)
  ncol_stars <- toupper(gsub("ind", "I", gsub("_.*", "", col_stars)))
  nn <- c(x$names, stats::setNames(ncol_stars,
                                   gsub("\\*", "\\\\*", col_stars)))
  match_names <- vapply(names(nn), 
                        function(x){
                          out <- which(grepl(x, names(xx)))
                          if(length(out) == 0L){NA}else{out}
                        }, 
                        integer(1), USE.NAMES = FALSE)
  setnames(xx, names(xx)[match_names[!is.na(match_names)]],
           as.character(nn[!is.na(match_names)]))
  res <- xx[, as.character(nn[!is.na(match_names)]), with = FALSE]
  if(grepl("#",res[1,ID],fixed=TRUE))
    res[,c("ID","DVID") := tstrsplit(ID,"#")][,c("ID","DVID"):=list(as.integer(ID),as.integer(DVID))]
  res
}


#' Read MONOLIX parameter estimation file
#'
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object
#' @importFrom utils read.table
#' @import data.table
#' @export
read_mlx_par_est <- function(path, x){
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
#'
#' @return data.table
#' @import data.table
#' @export
load_data_set <- function(x, path, sys){
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
    return(do.call(x[["reader"]], list(fpath, x)))
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
#' @param include list of data sets to load,
#' if missed ( the default behavior) load all sources.
#' @param exclude list of data sets to exclude from loading
#'
#' @return list of data.table
#' @export
load_source <- function(sys, path, dconf, include, exclude){
  names. <- names(dconf)
  if(!missing(include)) names. <- include
  if(!missing(exclude)) names. <- setdiff(names., exclude)
  
  datasets <- dconf[names.]
  dxs <- lapply(datasets,
                load_data_set,
                path = path,
                sys = sys)
  dxs
}
#' Merge input and fingrid data sets
#'
#' @param input \code{data.table} input data set
#' @param finegrid \code{data.table} finegrid data set
#' @param covariates \code{character} covariates names vector (optional)
#' @return data.table
#' @importFrom zoo na.locf

input_finegrid <- function(input, finegrid, covariates = NULL)
{
  ## this for check purpose
  ID <- TIME <- DVID <- NULL
  dx <- rbind(finegrid, input, fill = TRUE)
  dx <- dx[order(ID, TIME)]
  dx <- dx[TIME>0]
  measures <- c("DV","PRED","IPRED")
  dx[,(measures):=lapply(.SD, na.locf,na.rm=FALSE), by=DVID,.SDcols = measures]
  if(!is.null(covariates))
    dx[, (covariates) := lapply(.SD, na.locf,na.rm=FALSE), .SDcols = covariates]
  dx
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
  setnames(ds, toupper(gsub("_|[0-9]+", "", names(ds))))
  ds[, c("VAR", "EFFECT", "FUN") := 
       list(gsub("_.*","",VARIABLE),
            gsub("eta_(.*)_.*","\\1",VARIABLE),
            gsub(".*_","",VARIABLE))]
  ds
}

post_load <- function(dxs, input, sys, dplot,...){
  ## avoid RCMDCHECK
  DVID <- ID <- NULL
  ## merge finegrid with input data 
  dxs[["IND"]] <- 
    if(!is.null(dxs[["finegrid"]])){
      input_finegrid(input,dxs[["finegrid"]],...)
    }else{
      message("No finegrid file: we use instead predictions.txt for individual plots")
      dxs[["predictions"]] 
    }  
  
  if(sys == "mlx"){
    ## add startification column
    vv <- names(dxs$eta)[vapply(dxs$eta, is.integer, TRUE)]
    ## prepare data set for stratification
    if(!"DVID" %in% names(dxs$predictions))
      dxs$predictions[,DVID:=1]
    mdx <- try(
      merge(dxs$predictions[, !"DV", with = FALSE], input, 
            by = c("ID", "TIME","DVID"))
      ,silent=TRUE)
    if(inherits(mdx,"try-error"))
      stop("error cannot merge predictions data with the modelling input")
    dxs$predictions <- mdx
    ## prepare data set for stratification
    if(!is.null(dxs$eta))
      dxs$eta <- post_load_eta(dxs$eta,input,sys)
    
    
    ## add shrinkage data set
    if(!is.null(dxs[["estimates"]]) && !is.null(dxs[["eta"]]))
      dxs[["shrink"]] <- 
      shrinkage(dxs[["estimates"]], dxs[["eta"]], sys = sys)
  }
  
  dxs
}
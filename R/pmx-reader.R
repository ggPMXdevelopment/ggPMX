#' Read MONOLIX individual parameters
#' @param path character path to the file
#' @param x dataset object
#' @param ... extra paramter not used
#'
#' @return data.table object
#' @import data.table

#' @export
read_mlx_ind_est <- function(path, x, ...) {
  ID <- OCC <- NULL
  ds <- pmx_fread(path)
  nn <- grep(
    "^id|^eta_.*_(mode|mean)$", names(ds),
    ignore.case = TRUE, value = TRUE
  )
  ds <- ds[, nn, with = FALSE]
  setnames(ds, grep("^id$", names(ds), ignore.case = TRUE, value = TRUE), "ID")
  ## remove all null variables
  ## NO RANDOM EFFECT
  ## TODO : treat case where we have epsilon as random effect
  valid_cols <-
    c(
      "ID",
      ds[, setdiff(names(ds), c("ID")), with = FALSE][, names(.SD)[colSums(.SD) != 0]]
    )
  ds <- ds[, valid_cols, with = FALSE]
  ## remove hash
  if (grepl("#", ds[1, ID], fixed = TRUE)) {
    ds[, c("ID", "OCC") := tstrsplit(ID, "#")       ][
      ,
      c("ID", "OCC") := list(as.integer(ID), as.integer(OCC))
      ]
  }
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
#' @param occ \emph{[Optional]}\code{character} inter individual occasion varaibles
#' @param endpoint \code{integer}  null in case of a single endpoint otherwise the index of endpoints.

#'
#' @return data.table well formatted containing modelling input data
#'
read_input <- function(ipath, dv, dvid, cats = "", conts="", strats="", occ="",endpoint=NULL) {
  DVID <- TIME <- EVID <- MDV <- NULL
  xx <- pmx_fread(ipath)
  
  id_col <- grep("^id$", names(xx), ignore.case = TRUE, value = TRUE)
  if(length(id_col)==0){
    id_col <- names(xx)[1]
    message("input do not contain ID variable: ggPMX use first input variable ",id_col)
  }
  setnames(xx, id_col , "ID")
  
  if (dv %in% names(xx)) {
    setnames(xx, dv, "DV")
  } else {
    dv.names <- paste(setdiff(names(xx), c("ID", "id", "time", "TIME")), collapse = " or ")
    dv.names <- sprintf("'%s'", dv.names)
    err.msg <- sprintf("%s : is not a valid measurable variable
                        suggested names are : %s", dv, dv.names)
    stop(err.msg)
  }
  
  if (nzchar(occ) && occ %in% names(xx)) {
    setnames(xx, occ, "OCC")
  }
  ## round time column for further merge
  setnames(xx, grep("^time$", names(xx), ignore.case = TRUE, value = TRUE), "TIME")
  xx[, TIME := round(TIME, 4)]
  
  setnames(xx, toupper(names(xx)))
  if (all(c("MDV", "EVID") %in% names(xx))) {
    xx <- xx[!(EVID == 1 & MDV == 1)]
  }
  if (dvid %in% names(xx)) {
    setnames(xx, dvid, "DVID")
    nb.end <- length(unique(xx[,DVID]))
    if(!is.null(endpoint) & nb.end >1){
      xx[,DVID :=as.integer(factor(y,labels = seq_along(unique(DVID))))]
      xx <- xx[DVID==endpoint]
    }
    
  } else { ## dummy variable for single endpoint case
    xx[, "DVID" := 1]
  }
  
  covariates <- unique(c(cats, conts))
  if (length(covariates[covariates != ""])) {
    covariates <- covariates[covariates != ""]
    if (any(!covariates %in% names(xx))) {
      stop(sprintf(
        "%s : is not a valid covariate variable\n",
        covariates[!covariates %in% names(xx)]
      ))
    }
  }
  if (length(cats[cats != ""]) > 0) {
    cats <- cats[cats != ""]
    xx[, (cats) := lapply(.SD, as.factor), .SDcols = cats]
  }
  if (length(strats[strats != ""]) > 0) {
    strats <- strats[strats != ""]
    xx[, (strats) := lapply(.SD, as.factor), .SDcols = strats]
  }
  if (length(conts[conts != ""]) > 0) {
    conts <- conts[conts != ""]
    xx[, (conts) := lapply(.SD, as.numeric), .SDcols = conts]
  }
  
  xx
  
  
  
}

mlx_ipred <- function(x){
  if("indpred_mode" %in% x) return("indpred_mode")
  if("indpred_mean" %in% x) {
    message("NO indpred_mode found use indpred_mean instead")
    return("indpred_mean")
  }
  if("indpred_mean*" %in% x) {
    message("NO indpred_(mode|mean) found use indpred_mean* instead")
    return("indpred_mean*")
  }
  message("NO valid mapping for IPRED")
  return(NULL)
}

mlx_iwres<- function(x){
  if("indwres_mode" %in% x) return("indwres_mode")
  if("indwres_mean" %in% x) {
    message("NO indwres_mode found use indwres_mean instead")
    return("indpred_mean")
  }
  if("indwres_mean*" %in% x) {
    message("NO indwres_(mode|mean) found use indwres_mean* instead")
    return("indwres_mean*")
  }
  message("NO valid mapping for IWRES")
  return(NULL)
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
read_mlx_pred <- function(path, x, ...) {
  ID <- DVID <- OCC <- NULL
  xx <- pmx_fread(path)
  setnames(xx, tolower(names(xx)))
  ## use configuration columns
  ids <- x$names %in% names(xx)
  ipred <- get(x$names$IPRED)(names(xx))
  nn <- as.character(c(x$names[ids],ipred))
  names.nn <- c(names(x$names[ids]),"IPRED")
  if("IWRES" %in% names(x$names)){
    iwres <- get(x$names$IWRES)(names(xx))
    nn <- c(nn,iwres)
    names.nn <- c(names.nn,"IWRES")
  }
  res <- setnames(xx[, nn, with = FALSE],names.nn)
  
  ## select columns
  
  if (grepl("#", res[1, ID], fixed = TRUE)) {
    res[, c("ID", "OCC") := tstrsplit(ID, "#")][, c("ID", "OCC") := list(as.integer(ID), as.integer(OCC))]
  }
  
  dvid <- as.list(match.call(expand.dots = TRUE))[-1]$dvid
  if (is.null(dvid) || !dvid %in% names(res)) {
    res[, "DVID" := 1]
  } else {
    setnames(res, dvid, "DVID")
  }
  
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
read_mlx_par_est <- function(path, x, ...) {
  xx <- setDT(read.table(path, sep = ";", header = TRUE))
  if ("names" %in% names(x)) {
    setnames(xx, x[["names"]])
  }
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
load_data_set <- function(x, path, sys, ...) {
  fpath <- file.path(path, x[["file"]])
  if (!file.exists(fpath)) {
    endpoint <- list(...)$endpoint
    if(!is.null(endpoint) && !is.null(x$pattern)){
      file_name <- sub("_",endpoint,x[["pattern"]])
      fpath <- file.path(path, file_name)
      message("use ",file_name, " for endpoint ",endpoint )
    }
  }
  if(!file.exists(fpath)){
    message("file ",x[["file"]], " do not exist")
    return(NULL)
  }
  
  
  
  if (exists("reader", x)) {
    return(do.call(x[["reader"]], list(fpath, x, ...)))
  }
  ds <- pmx_fread(fpath)
  ds <- ds[, !grep("^V[0-9]+", names(ds)), with = FALSE]
  data.table::setnames(ds, tolower(names(ds)))
  if ("names" %in% names(x)) {
    setnames(
      ds,
      tolower(names(x[["names"]])),
      as.character(x[["names"]])
    )
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
load_source <- function(sys, path, dconf, ...) {
  names. <- names(dconf)
  DVID <- NULL
  
  
  
  pk_pd <- c("predictions1","predictions2","finegrid1","finegrid2")
  datasets <- dconf[setdiff(names.,pk_pd)]
  use_finegrid <- list(...)$use_finegrid
  nn <- names(datasets)
  if(!use_finegrid)
    nn <- setdiff(nn,"finegrid")
  dxs <- lapply(datasets[nn], function(x) {
    load_data_set(x, path = path, sys = sys, ...)
  })
  
  ## case multi endpoints
  if(sum(grepl("predictions",names(dconf)))>0){
    if(is.null(dxs[["predictions"]])){
      datasets <- dconf[pk_pd]
      dxs2 <- lapply(datasets, function(x) {
        load_data_set(x, path = path, sys = sys, ...)
      })
      
      endpoint = list(...)$endpoint
      if(is.null(endpoint)) endpoint <- 1
      if(!is.null(dxs[["eta"]]))dxs[["eta"]][,DVID:=endpoint]
      if(!is.null(dxs2[["predictions1"]]) && !is.null(dxs2[["predictions2"]])){
        dxs[["predictions"]] <- dxs2[[sprintf("predictions%s",endpoint)]]
        dxs[["predictions"]][,DVID:=endpoint]
      }
      if(!is.null(dxs2[["finegrid1"]]) && !is.null(dxs2[["finegrid2"]])){
        dxs[["finegrid"]] <- dxs2[[sprintf("finegrid%s",endpoint)]]
        dxs[["finegrid"]][,DVID:=endpoint]
      }
    }
    for ( x in setdiff(names.,pk_pd))
      if (is.null(dxs[[x]]))
        message(sprintf(" %s FILE DOES NOT exist", x))
  }
  
  
  dxs
}

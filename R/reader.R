#' Read MONOLIX individual parameters
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object

#' @export
read_mlx_ind_est <- function(path, x){
  ds <- fread(path)
  ds <- ds[, !grep("^V[0-9]+", names(ds)), with = FALSE]
  data.table::setnames(ds, tolower(names(ds)))
  
  ds <- data.table::melt(ds, measure = grep("_.*_", names(ds)))
  data.table::setnames(ds, toupper(gsub("_|[0-9]+", "", names(ds))))
  VARIABLE <- NULL
  ds[, c("VAR", "EFFECT", "FUN") := data.table::tstrsplit(VARIABLE, "_")]
  
}


#' Read MONOLIX input data 
#'
#' @param ipath path +filename to the input file
#' 
#' @return data.table well formatted
#' @import data.table
#' @export
#'
read_input <- function(ipath,dv=NULL,covariates=""){
  xx <- fread(ipath)
  setnames(xx, toupper(names(xx)))
  if(!is.null(dv))setnames(xx,dv,"DV")
  
}

#' Read MONOLIX model predictions
#'
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object

#' @export
read_mlx_pred <- function(path, x){
  xx <- fread(path)
  data.table::setnames(xx, tolower(names(xx)))
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
  xx[, as.character(nn[!is.na(match_names)]), with = FALSE]
}


#' Read MONOLIX parameter estimation file
#'
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object
#' @importFrom utils read.table
#' @export
read_mlx_par_est <- function(path, x){
  xx <- data.table::setDT(read.table(path, sep = ";", header = TRUE))
  if("names" %in% names(x))
    setnames(xx, x[["names"]])
  xx
}

#' Load data set
#'
#' @param x data set config
#' @param path character path to the directory
#'
#' @return data.table
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
    warning(sprintf(" %s FILE DOES NOT exist under %s", x[["file"]], path))
    return(NULL)
  }
  
  
  if(exists("reader", x))
    return(do.call(x[["reader"]], list(fpath, x)))
  ds <- fread(fpath)
  ds <- ds[,!grep("^V[0-9]+", names(ds)), with = FALSE]
  data.table::setnames(ds, tolower(names(ds)))
  if("names" %in% names(x)){
    data.table::setnames(ds,
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

input_finegrid <- function(input,finegrid,covariates=NULL)
{
  dx <- rbind(finegrid,input,fill=TRUE)
  dx <- dx[order(ID,TIME)]
  if(!is.null(covariates))
    dx[,(covariates):=lapply(.SD,na.locf),.SDcols=covariates]
  dx[TIME>0]
}


post_load <- function(dxs,input, sys, dplot,...){
  ## merge finegrid with input data 
  dxs[["ind_pred"]] <- 
    if(!is.null(dxs[["finegrid"]])) input_finegrid(dxs[["finegrid"]],input,...)
  else  dxs$ind_pred 
  
  if(sys == "mlx"){
    ## add startification column
    vv <- names(dxs$ind_pred)[vapply(dxs$ind_pred, is.integer, TRUE)]
    ## prepare data set for stratification
    dxs$mod_pred <-
      merge(dxs$mod_pred,
            unique(dxs$ind_pred[, vv, with = FALSE]),
            by = "ID")
    ## add shrinkage data set
    dxs[["shrink"]] <- 
      shrinkage(dxs[["par_est"]], dxs[["ind_pred"]], sys = sys)
  }

  dxs
}



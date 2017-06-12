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
  data.table::setnames(ds, tolower(names(ds)))
  ds <- melt(ds, measure = grep("_.*_", names(ds)))
  data.table::setnames(ds, toupper(gsub("_|[0-9]+", "", names(ds))))
  VARIABLE <- NULL
  ds[, c("VAR", "EFFECT", "FUN") := data.table::tstrsplit(VARIABLE, "_")]
  
}


#' Read Modelling input data 
#'
#' @param ipath full path of the input file
#' 
#' @return data.table well formatted containing modelling input data
#' @export
#'
read_input <- function(ipath, dv = NULL,dvid, covariates = ""){
  xx <- pmx_fread(ipath)
  setnames(xx, toupper(names(xx)))
  dv <- toupper(dv)
  dvid <- toupper(dvid)
  if(dv %in% names(xx)) setnames(xx, dv, "DV")
  else stop(sprintf("%s : is not a valid measurable variable",dv))
  if(dvid %in% names(xx)) setnames(xx, dvid, "DVID")
  else xx[,DVID:=1]
  if(length(covariates)>0){
    covariates <- toupper(covariates)
    if(any(!covariates %in% names(xx))) 
      stop(sprintf("%s : is not a valid covariate variable\n",
                   covariates[!covariates%in%names(xx)]))
  }
  xx
  
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
  xx[, as.character(nn[!is.na(match_names)]), with = FALSE]
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
    warning(sprintf(" %s FILE DOES NOT exist under %s", x[["file"]], path))
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

input_finegrid <- function(input, finegrid, covariates = NULL)
{
  dx <- rbind(finegrid, input, fill = TRUE)
  dx <- dx[order(ID, TIME)]
  if(!is.null(covariates))
    dx[, (covariates) := lapply(.SD, na.locf), .SDcols = covariates]
  dx[TIME > 0]
}


post_load <- function(dxs, input, sys, dplot,...){
  ## merge finegrid with input data 
  dxs[["IND"]] <- 
    if(!is.null(dxs[["finegrid"]])){
      input_finegrid(dxs[["finegrid"]], input,...)
    }else{
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
    ## add shrinkage data set
    if(!is.null(dxs[["estimates"]]) && !is.null(dxs[["eta"]]))
      dxs[["shrink"]] <- 
      shrinkage(dxs[["estimates"]], dxs[["eta"]], sys = sys)
  }
  
  dxs
}



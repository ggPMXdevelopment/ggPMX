read_mlx_ind_est <- function(path,x){
  ds <- fread(path)
  ds <- ds[,!grep("^V[0-9]+",names(ds)),with=FALSE]
  setnames(ds,tolower(names(ds)))

  ds <- melt(ds,measure=grep("_.*_",names(ds)))
  setnames(ds,toupper(gsub("_|[0-9]+","",names(ds))))
  ds[,c("VAR","EFFECT","FUN"):=tstrsplit(VARIABLE,"_")]

}

#' Read MONOLIX model predictions
#'
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object

#' @export
read_mlx_pred <- function(path,x){
  xx <- fread(path)
  setnames(xx,tolower(names(xx)))
  ## mean start columns
  col_stars <- grep("*",names(xx),fixed=TRUE,value=TRUE)
  ncol_stars <- toupper(gsub("ind","I",gsub("_.*","",col_stars)))
  nn <- c(x$names,setNames(ncol_stars,col_stars))
  nn <- nn[names(nn)%in%names(xx)]

  setnames(xx,names(nn),as.character(nn))
  xx[,as.character(nn),with=FALSE]
}


#' Read MONOLIX parameter estimation file
#'
#' @param path character path to the file
#' @param x dataset object
#'
#' @return data.table object
#' @importFrom utils read.table
#' @export
read_mlx_par_est <- function(path,x){
  xx <- setDT(read.table(path,sep=";",header=TRUE))
  if("names" %in% names(x))
    setnames(xx,x[["names"]])
  xx
}

#' Load data set
#'
#' @param x data set config
#' @param path character path to the directory
#'
#' @return data.table
#' @export
#' @import data.table
load_data_set <- function(x,path,sys){
  fpath <- file.path(path,x[["file"]])
  if(!file.exists(fpath)){
    fpath <- grep(x[["file"]],list.files(path,full=TRUE),value=TRUE)
    if(length(fpath)>1)
      fpath <- grep(sys,fpath,ignore.case = TRUE,value=TRUE)
  }
  if(!file.exists(fpath)){
    cat(sprintf(" %s FILE DO NOT exist under %s",x[["file"]],path))
    return(NULL)
  }


  if(exists("reader",x))
    return(do.call(x[["reader"]],list(fpath,x)))
  ds <- fread(fpath)
  ds <- ds[,!grep("^V[0-9]+",names(ds)),with=FALSE]
  setnames(ds,tolower(names(ds)))
  if("names" %in% names(x)){
    setnames(ds,tolower(names(x[["names"]])),as.character(x[["names"]]))
    ds <- ds[,as.character(x[["names"]]),with=FALSE]
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
load_source <- function(sys,path,dconf,include,exclude){
  names. <- names(dconf)
  if(!missing(include)) names. <- include
  if(!missing(exclude)) names. <- setdiff(names.,exclude)

  datasets <- dconf[names.]
  dxs <- lapply(datasets,
                load_data_set,
                path=path,
                sys=sys)
  dxs
}


post_load <- function(dxs,sys,dplot){
  mm <- unlist(dplot)
  if(sys=="mlx"){
   ## add startification column
       vv <- names(dxs$ind_pred)[vapply(dxs$ind_pred,is.integer,T)]
      ## prepare data set for stratification
      dxs$mod_pred <-
        merge(dxs$mod_pred,
              unique(dxs$ind_pred[,vv,with=FALSE]),
              by="ID")
   ## add shrinkage data set
   dxs[["shrink"]] <- shrinkage(dxs[["par_est"]],dxs[["ind_pred"]],sys = sys)
  }
  dxs
}



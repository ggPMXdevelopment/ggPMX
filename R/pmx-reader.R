#' Read MONOLIX individual parameters
#' @param path character path to the file
#' @param x dataset object
#' @param ... extra paramter not used
#'
#' @return data.table object
#' @import data.table

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

read_mlx18_ind_est <- function(path, x, ...) {
  
  path <- file.path(dirname(path),x$subfolder,x$file)
  read_mlx_ind_est(path,x,...)
  
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
read_input <- function(ipath, dv, dvid, cats = "", conts="", strats="", occ="", endpoint=NULL) {
  TIME <- EVID <- MDV <- y <- NULL
  xx <- pmx_fread(ipath)

  if (all(c("MDV", "EVID") %in% toupper(names(xx)))) {
    setnames(xx, grep("^mdv$", names(xx), ignore.case = TRUE, value = TRUE), "MDV")
    setnames(xx, grep("^evid$", names(xx), ignore.case = TRUE, value = TRUE), "EVID")
    xx <- xx[!(EVID == 1 & MDV == 1)]
  }


  if (!is.null(endpoint)) {
    if (!is.null(dvid) && dvid %in% names(xx)) {
      rr <- dvid
      xx <- xx[get(rr) == endpoint$code]
      if (!nrow(xx)) {
        msg <- sprintf("No observations data for endpoint %s\n", endpoint$code)
        stop(msg)
      }
    } else {
      msg <- sprintf("ggPMX can not filter by endpoint %s\n", endpoint$code)
      msg <- paste(msg, sprintf("%s is not a valid column in the observation data set", dvid))

      stop(msg)
    }
  }
  else {
    if (!is.null(dvid) && dvid %in% names(xx)) {
      rr <- dvid
      ends <- unique(xx[, get(rr)])
      if (length(ends) > 1) {
        msg <- sprintf("Observation data contains multiple endpoints %s. \n ", paste(ends, collapse = " ; "))
        msg <- paste(msg, "Please select a single endpoint to continue.")

        stop(msg)
      }
    }
  }




  id_col <- grep("^id$", names(xx), ignore.case = TRUE, value = TRUE)
  if (length(id_col) == 0) {
    id_col <- names(xx)[1]
    message("input do not contain ID variable: ggPMX use first input variable ", id_col)
  }
  setnames(xx, id_col, "ID")

  if (dv %in% names(xx)) {
    if(dv=="dv") xx[,DV:=dv]
    else xx[,DV:=get(dv)]
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

mlx_ipred <- function(x) {
  if ("indpred_mode" %in% x) return("indpred_mode")
  if ("indpred_mean" %in% x) {
    message("NO indpred_mode found use indpred_mean instead")
    return("indpred_mean")
  }
  if ("indpred_mean" %in% x) {
    message("NO indpred_(mode|mean) found use indpred_mean* instead")
    return("indpred_mean*")
  }
  message("NO valid mapping for IPRED")
  return(NULL)
}


mlx18_ipred <- function(x) {
  if ("indivpred_mode" %in% x) return("indivpred_mode")
  if ("indivpred_mean" %in% x) {
    message("NO indivpred_mode found use indivpred_mean instead")
    return("indivpred_mean")
  }
  message("NO valid mapping for IPRED")
  return(NULL)
}

mlx_iwres <- function(x) {
  if ("indwres_mode" %in% x) return("indwres_mode")
  if ("indwres_mean" %in% x) {
    message("NO indwres_mode found use indwres_mean instead")
    return("indpred_mean")
  }
  if ("indwres_mean*" %in% x) {
    message("NO indwres_(mode|mean) found use indwres_mean* instead")
    return("indwres_mean*")
  }
  message("NO valid mapping for IWRES")
  return(NULL)
}

mlx18_iwres <- function(x) {
  if ("indwres_mode" %in% x) return("indwres_mode")
  if ("indwres_mean" %in% x) {
    message("NO indwres_mode found use indwres_mean instead")
    return("indpred_mean")
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

read_mlx_pred <- function(path, x, ...) {
  ID <- OCC <- NULL
  xx <- pmx_fread(path)
  setnames(xx, tolower(names(xx)))
  ## use configuration columns
  ids <- x$names %in% names(xx)
  ipred <- get(x$names$IPRED)(names(xx))
  nn <- as.character(c(x$names[ids], ipred))
  names.nn <- c(names(x$names[ids]), "IPRED")
  if ("IWRES" %in% names(x$names)) {
    iwres <- get(x$names$IWRES)(names(xx))
    nn <- c(nn, iwres)
    names.nn <- c(names.nn, "IWRES")
  }
  res <- setnames(xx[, nn, with = FALSE], names.nn)

  ## select columns

  if (grepl("#", res[1, ID], fixed = TRUE)) {
    res[, c("ID", "OCC") := tstrsplit(ID, "#")][, c("ID", "OCC") := list(as.integer(ID), as.integer(OCC))]
  }

  res
}


read_mlx18_res <- function(path, x, ...) {
  
  if (exists("subfolder",x))
    path <- file.path(dirname(path),x$subfolder)
  
  res_file <- file.path(path,x$file)
  file_path <- if(!file.exists(res_file)){
    list.files(path,pattern=x$pattern,full.names = TRUE)[1]
  } else res_file
  if(!file.exists(file_path)){
    message(sub(".txt", "", x[["file"]]), " file do not exist")
    return(NULL)
  }
  
  ds <- pmx_fread(file_path)
  ids <- match(tolower(names(x[["names"]])),tolower(names(ds)))
  new_vars <- names(x[["names"]])
  setnames(ds, ids,new_vars)
  ds[,new_vars,with=FALSE]

}

read_mlx18_pred <- function(path, x, ...) {
  
  ds <- read_mlx_pred(path=path,x=x,...)
  resi <-  read_mlx18_res(path,x$residuals)
  merge(ds,resi)
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
load_data_set <- function(x, path, sys, ...) {
  
  
  fpath <- file.path(path, x[["file"]])
  if (!file.exists(fpath) ) {
    endpoint <- list(...)$endpoint
    if (!is.null(endpoint) && !is.null(x$pattern)) {
      if (!is.null(endpoint$files)) {
        ff <- endpoint$files
        file_name <- sprintf("%s.txt", ff[[x[["pattern"]]]])
      } else {
        file_name <- sprintf("%s%s.txt", x[["pattern"]], endpoint$code)
      }

      fpath <- file.path(path, file_name)
      if (length(fpath) > 0 && file.exists(fpath)) {
        message("use ", file_name, " for endpoint ", endpoint$code)
      }
    }
  }
  
  if(sys!="mlx18"){
    if (length(fpath) == 0 || !file.exists(fpath)) {
      message(sub(".txt", "", x[["file"]]), " file do not exist")
      return(NULL)
    }
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
  dxs <- lapply(dconf, function(x) {
    load_data_set(x, path = path, sys = sys, ...)
  })


  dxs
}



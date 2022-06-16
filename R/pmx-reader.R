#' Read MONOLIX individual parameters
#' @param path character path to the file
#' @param x dataset object
#' @param ... extra parameter not used
#'
#' @return data.table object
#' @import data.table

read_mlx_ind_est <- function(path, x, ...) {
  ID <- OCC <- NULL
  ds <- pmx_fread(path)
  if(!is.null(x$id) && exists(x$id,ds)) setnames(ds,x$id,"id")

  occ <- list(...)$occ
  if (is.null(occ)) occ <- ""
  patt_fields <- "^id|^%s|^eta_.*_(mode|mean)$"
  nn <- grep(
    patt_fields, names(ds),
    ignore.case = TRUE, value = TRUE
  )
  if (occ != "" && any(grepl(occ, names(ds), ignore.case = TRUE))) nn <- c(nn, occ)
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
  if (!is.null(occ) && occ %in% names(ds)) setnames(ds, occ, "OCC")
  if (identical(names(ds), "ID")) {
    message("eta file is provided but no random effects:probably all columns are null.")
    return(NULL)
  }
  ds
}

read_mlx18_ind_est <- function(path, x, ...) {
  path <- file.path(dirname(path), x$subfolder, x$file)
  read_mlx_ind_est(path, x, ...)
}



#' Read Modelling input data
#'
#' @param ipath full path of the input file
#' @param dv \code{character} the name of measurable variable used in the input modelling file
#' @param dvid \code{character} observation type parameter
#' @param cats \emph{[Optional]}\code{character} vector of categorical covariates
#' @param conts \emph{[Optional]}\code{character} vector of continuous covariates
#' @param strats \emph{[Optional]}\code{character} extra stratification variables
#' @param occ \emph{[Optional]}\code{character} inter individual occasion variables
#' @param endpoint \code{integer}  null in case of a single endpoint otherwise the index of endpoints.
#' @param id \code{character} the name of identifier variable used in the input modelling file.
#' @param time \code{character} the name of time variable used in the input modelling file

#'
#' @return data.table well formatted containing modelling input data
#'
read_input <- function(ipath, dv, dvid, cats = "", conts = "", strats = "", occ = "",
                       endpoint = NULL, id = NULL, time = NULL) {
  TIME <- EVID <- MDV <- y <- DV <- ID <- OCC <- NULL
  xx <- pmx_fread(ipath)

  if (!is.null(id) && !exists(id,xx)) {
    stop(sprintf("observation data does not contain id variable: %s",id))
  }
  if (!is.null(time) && !exists(time,xx)) {
    stop(sprintf("observation data does not contain time variable: %s",time))
  }

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



  if (!is.null(id) ) {
    if(id!="ID"){
      if (exists("ID",xx))xx[,ID:=NULL]
      setnames(xx, id, "ID")
    }
  } else {
    id_col <- grep("^id$", names(xx), ignore.case = TRUE, value = TRUE)
    if (length(id_col) == 0) {
      id_col <- names(xx)[1]
      message("input do not contain ID variable: ggPMX use first input variable ", id_col)
    }
    setnames(xx, id_col, "ID")
  }

  if (dv %in% names(xx)) {
    if (dv == "dv") {
      xx[, DV := dv]
    } else {
      xx[, DV := get(dv)]
    }
    # Omitting 0-value observations for compatibility with log transformations
    #xx <- xx[DV != 0]
  } else {
    dv.names <- paste(setdiff(names(xx), c("ID", "id", "time", "TIME")), collapse = " or ")
    dv.names <- sprintf("'%s'", dv.names)
    err.msg <- sprintf("%s : is not a valid measurable variable
                        suggested names are : %s", dv, dv.names)
    stop(err.msg)
  }
    if (nzchar(occ) && occ %in% names(xx)) {
      if(occ!="OCC"){
        if(exists("OCC",xx))xx[,OCC:=NULL]
        setnames(xx, occ, "OCC")
      }
  }
  ## round time column for further merge
  if(!is.null(time)){
    if (time!="TIME" ){
    if (exists("TIME",xx))xx[,TIME:=NULL]
    setnames(xx, time, "TIME")
    }
  } else {
    setnames(xx, grep("^time$", names(xx), ignore.case = TRUE, value = TRUE), "TIME")
  }
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
  if ("indpred_mode" %in% x) {
    return("indpred_mode")
  }
  if ("indpred_mean" %in% x) {
    message("NO indpred_mode found use indpred_mean instead")
    return("indpred_mean")
  }
  if ("indpred_mean*" %in% x) {
    message("NO indpred_(mode|mean) found use indpred_mean* instead")
    return("indpred_mean*")
  }
  message("NO valid mapping for IPRED")
  return(NULL)
}


mlx18_ipred <- function(x) {
  if ("indivpred_mode" %in% x) {
    return("indivpred_mode")
  }
  if ("indivpred_mean" %in% x) {
    message("NO indivpred_mode found use indivpred_mean instead")
    return("indivpred_mean")
  }
  message("NO valid mapping for IPRED")
  return(NULL)
}

mlx18_finegrid_ipred <- function(x) {
  if ("indivpredmode" %in% x) {
    return("indivpredmode")
  }
  if ("indivpredmean" %in% x) {
    message("NO indivPredMode found use indivPredMean instead")
    return("indivpredmean")
  }
  message("NO valid mapping for IPRED")
  return(NULL)
}


mlx_iwres <- function(x) {
  if ("indwres_mode" %in% x) {
    return("indwres_mode")
  }
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
  if ("indwres_mode" %in% x) {
    return("indwres_mode")
  }
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
#' @param ... extra parameter not used
#'
#' @return data.table object
#' @import data.table

read_mlx_pred <- function(path, x, ...) {
  ID <- OCC <- id <- NULL
  if (!file.exists(path)) {
    message(sub(".txt", "", x[["file"]]), " file do not exist")
    return(NULL)
  }
  xx <- pmx_fread(path)
  if(!is.null(x$id) && exists(x$id,xx)) setnames(xx,x$id,"id")

  setnames(xx, tolower(names(xx)))
  id_col <- grep("^id", names(xx), ignore.case = TRUE, value = TRUE)
  if (length(id_col) > 0 && nzchar(id_col)) setnames(xx, id_col, "id")
  if (grepl("#", xx[1, "id", with = FALSE], fixed = TRUE)) {
    xx[, c("id", "OCC") := tstrsplit(id, "#")][, c("id", "OCC") := list(as.integer(id), as.integer(OCC))]
  }
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
  occ <- list(...)$occ
  if (is.null(occ)) occ <- ""
  if ("OCC" %in% names(xx)) {
    nn <- c(nn, "OCC")
    names.nn <- c(names.nn, "OCC")
  }
  if (occ != "" && !"OCC" %in% names(xx)) {
    nn <- c(nn, tolower(occ))
    names.nn <- c(names.nn, "OCC")
  }
  res <- setnames(xx[, nn, with = FALSE], names.nn)

  ## select columns



  res
}
#also reads mlx19
read_mlx18_res <- function(path, x, ...) {

  # Path should not have duplicated structure of subdirectory, but if not present,
  # it should be added.
  if(exists("subfolder", x) & !(grepl(x[["subfolder"]], dirname(path), fixed=TRUE))) {
    path <- file.path(dirname(path), x[["subfolder"]], "/")
  }

  path <- dirname(path)

  res_file <- file.path(path, x$file)

  file_path <- if (!file.exists(res_file)) {
    ffiles <- list.files(path, pattern = x$pattern, full.names = TRUE)
    if (!is.null(x$endpoint)) {
      fpath <- ffiles[grep(x$endpoint, basename(ffiles))][1]
    } else {
      fpath <- ffiles[1]
    }
    if (is.na(fpath)) {
      message(sub(".txt", "", x[["file"]]), " file do not exist")
      return(NULL)
    }
    cat("use ", basename(fpath), " as ", x$label, ".\n", sep = "")
    fpath
  } else {
    res_file
  }

  if (!file.exists(file_path)) {
    message(sub(".txt", "", x[["file"]]), " file do not exist")
    return(NULL)
  }

  ds <- pmx_fread(file_path)

  if(!is.null(x$id) && exists(x$id,ds)) setnames(ds,x$id,"id")

  if(x$pattern == "_obsVsPred") {
    xnames <- names(x[["names"]])
    yname <- substring(file_path, regexpr("s/", file_path) + 2)
    yname <- sub("_obsVsPred.txt", "", yname)
    names(x[["names"]])[which(xnames == "y_simBlq_mode")] <- paste0(yname,"_simBlq_mode")

    #handling of mlx18 input, there is no y_simBlq_mean or y_simBlq_mode for Monolix version 2018
    if(length(grep("simBlq_mode", names(ds))) == 0) {
      names(x[["names"]])  <- gsub("_mode","", names(x[["names"]]))
      name_simBlq <-  names(ds)[grep("simBlq", names(ds))]

    message(paste0("Using simulated BLOQs of Monolix 2018 can cause slight deviations from Monolix plots regarding simulated BLOQs of the DV!\n",
                   "Try Monolix 2019 or later for improved ggPMX simulated BLOQ function."))
    }

  }

  ids <- match(tolower(names(x[["names"]])), tolower(names(ds)))

  if(!is.null(x[["newnames"]])) {
    new_vars <- names(x[["newnames"]])
  } else {
    new_vars <- names(x[["names"]])
  }

  occ <- list(...)$occ
  if (is.null(occ)) occ <- ""
  if ("OCC" %in% names(ds)) {
    new_vars <- c(new_vars, "OCC")
    ids <- c(ids,grep("OCC", names(ds)))
  }

  if (occ != "" && !"OCC" %in% names(ds)) {
    new_vars <- c(new_vars, "OCC")
    ids <- c(ids,grep(occ, names(ds)))
  }

#if it doesn't work correctly, give null datatable instead of error
if(NA %in% ids){
  ds <- NULL
} else {
  setnames(ds, ids, new_vars)
  ds[, new_vars, with = FALSE]
}


}

read_mlx18_pred <- function(path, x, ...) {
  ID <- NULL
  if (exists("subfolder", x) && !file.exists(path)) {
    path <- file.path(dirname(path), x$subfolder)
    finegrid_file <- file.path(path, x$file)
    path <- if (!file.exists(finegrid_file)) {
      list.files(path, pattern = x$pattern, full.names = TRUE)[1]
    } else {
      finegrid_file
    }
  }


  ds <- read_mlx_pred(path = path, x = x, ...)
  if (exists("residuals", x)) {
    x$residuals$endpoint <- x$endpoint
    x$residuals$id <- x$id
    resi <- read_mlx18_res(path, x$residuals)
    if (is.null(resi)) {
      return(NULL)
    }
    if (inherits(ds$ID,"factor") & !inherits(resi$ID,"factor")) {
      resi[, ID := factor(ID, levels = levels(ID))]
    }
    if (!inherits(ds$ID, "factor") & inherits(resi$ID, "factor")) {
      ds[, ID := factor(ID, levels = levels(ID))]
    }
    ds <- merge(ds, resi, by = c("ID", "TIME"))

  }
  ds
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
  sep <- ifelse(exists("sep", x), x$sep, ";")
  xx <- setDT(read.table(path, sep = sep, header = TRUE))
  if ("names" %in% names(x)) {
    # This handles the case where the
    nam <- x[["names"]]
    do_more <- FALSE
    if (length(nam) > ncol(xx)) {
      nam <- nam[seq(1, ncol(xx))]
    }
    setnames(xx, seq_along(nam), nam)
    if (do_more) {
      nam <- x[["names"]]
      nam <- nam[-seq(1, ncol(xx))]
      xx <- xx[, (nam) := NA]
    }
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
  exists_file <- file.exists(fpath)

  params <- get_params_from_call()

  if (!exists_file) {
    ep <- list(...)$endpoint
    if (!is.null(ep) && !is.null(x$pattern)) {
      ffiles <- list.files(path, x$pattern, recursive = TRUE, full.names = TRUE)
      exists_file <- sum(grepl(ep$file.code, ffiles)) > 0
      if (exists_file) {
        fpath <-
          if (length(ffiles) > 1) {
            ffiles[grep(ep$file.code, basename(ffiles))][1]
          } else {
            ffiles[1]
          }
        x$endpoint <- ep$file.code
        cat("use ", basename(fpath), " as ", x$label, ".\n")
      }
    }
  }

  if (!exists_file && sys != "mlx18") {
    cat(x[["label"]], " file does not exist.\n")
    return(NULL)
  }

  if (exists("reader", x)) {
    return(do.call(x[["reader"]], list(fpath, x, ...)))
  }

  ds <- pmx_fread(fpath)
  if(!is.null(x$id) && exists(x$id,ds)) setnames(ds,x$id,"id")


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
  Map(function(x, nn) {
    x$name <- nn
    if(!is.null(list(...)$id)) x$id <- list(...)$id
    load_data_set(x, path = path, sys = sys, ...)
  }, dconf, names(dconf))
}

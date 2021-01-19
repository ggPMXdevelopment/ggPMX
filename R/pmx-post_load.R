#' Merge input and fingrid data sets
#'
#' @param input \code{data.table} input data set
#' @param finegrid \code{data.table} finegrid data set
#' @return data.table
#' @importFrom zoo na.locf

input_finegrid <- function(input, finegrid) {
  ## this for R CMD check purpose
  ID <- TIME <- NULL
  if (is.null(finegrid)) {
    return(NULL)
  }
  input[, source := "in"]
  dx <- rbind(finegrid, input, fill = TRUE)[order(ID, TIME)]

  measures <- setdiff(names(input), c("ID", "DV", "TIME", "source"))
  if (length(measures) > 0) {
    dx[, (measures) :=
      lapply(.SD, na.locf, na.rm = FALSE), by = "ID", .SDcols = measures]
  }
  input[, source := NULL]
  dx[is.na(source) & TIME >= 0][, source := NULL]
}


post_load_eta <- function(ds, input, sys, occ) {
  if (missing(occ)) occ <- ""
  ID <- DVID <- VARIABLE <- NULL
  keys <- c("ID")
  if (inherits(ds$ID,"factor") & !inherits(input$ID,"factor")) {
    input[, ID := factor(ID, levels = levels(ID))]
  }
  if (!inherits(ds$ID, "factor") & inherits(input$ID, "factor")) {
    ds[, ID := factor(ID, levels = levels(ID))]
  }
  if (occ != "") keys <- c(keys, if (length(occ) == 1) "OCC" else sprintf("OCC%s", seq_along(occ)))
  ds <- try(
    merge(
      ds, input,
      by = keys
    ),
    silent = TRUE
  )

  if (inherits(ds, "try-error")) {
    stop("error cannot merge eta data with the modelling input")
  }
  if (nrow(ds) == 0) {
    stop("error cannot merge eta data with the modelling input: no individual match")
  }
  ## put in the long format
  measures <- grep("eta_.*_", names(ds))
  if (length(measures) == 0) {
    message("NO random effect found")
    return(ds)
  }
  ds[, (measures) := lapply(.SD, as.numeric), .SDcols = measures]
  ds <- melt(ds, measure = measures)
  setnames(ds, "value", "VALUE")
  ## setnames(ds, toupper(names(ds)))
  ## keep only mean or mode variable
  variable <- NULL
  ds[grep("(mode|mean)$", variable)]
  ## reshape columns for easier filtering
  ds[, c("EFFECT", "FUN") :=
    list(
      gsub("eta_(.*)_(mode|mean)", "\\1", variable),
      gsub(".*_", "", variable)
    )]
  ds[, c("variable") := NULL]
  ds
}

post_load <- function(dxs, input, sys, dplot, occ) {
  ## avoid RCMDCHECK
  DVID <- ID <- NULL
  warns <- list()
  ## merge finegrid with input data
  if (sys %in% c("mlx", "mlx18")) {
    keys <- c("ID", "TIME")
    if (occ != "") keys <- c(keys, if (length(occ) == 1) "OCC" else sprintf("OCC%s", seq_along(occ)))
    
    if (!is.null(dxs[["predictions"]]) & !is.null(dxs[["sim_blq_npde_iwres"]]) & !is.null(dxs[["sim_blq_y"]])) {
      dxs[["sim_blq"]] <- merge(dxs[["sim_blq_npde_iwres"]], dxs[["sim_blq_y"]], by = keys)
      dxs[["sim_blq"]] <- merge(dxs[["sim_blq"]], input, by = keys)
      dxs[["sim_blq"]] <- merge(dxs[["sim_blq"]], dxs[["predictions"]], by = keys)
    }
    
    if (!is.null(dxs[["predictions"]])) {
      dxs[["predictions"]] <- merge(dxs[["predictions"]], input, by = keys)
    }
    
    if (!is.null(dxs[["finegrid"]])) {
      dxs[["finegrid"]] <- input_finegrid(input, dxs[["finegrid"]])
      dxs[["IND"]] <- dxs[["finegrid"]]
    }
    if (is.null(dxs[["finegrid"]]) && !is.null(dxs[["predictions"]])) {
      warn <-
        "NO FINEGRID FILE:
        we will use instead predictions.txt for individual plots"
      warns$MISSING_FINEGRID <- warn

      message(warn)
      dxs[["IND"]] <- dxs[["predictions"]]
    }
  }
  list(
    data = dxs,
    warnings = warns
  )
}

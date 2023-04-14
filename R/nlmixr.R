

#' Creates pmx controller from  an nlimxr fit object
#'
#' @param fit nlmixr object
#' @param dvid \emph{[Optional]} \code{character} observation type parameter.
#' @param conts \emph{[Optional]}\code{character} vector of continuous covariates
#' @param cats \emph{[Optional]}\code{character} vector of categorical covariates
#' @param strats \emph{[Optional]}\code{character} extra stratification variables
#' @param endpoint \code{pmxEndpointClass} or \code{integer} or \code{charcater} defalut to NULL
#' of the endpoint code.   \code{\link{pmx_endpoint}}
#' @param settings \emph{[Optional]}\code{pmxSettingsClass} \code{\link{pmx_settings}}
#' @param vpc \emph{[Optional]} \code{logical} a boolean indiacting if vpc should be calculated (by default \code{TRUE})

#' @return \code{pmxClass} controller object.
#' @export
#' @importFrom stats setNames
pmx_nlmixr <- function(fit, dvid, conts, cats, strats, endpoint, settings, vpc = FALSE) {
  EFFECT <- EVID <- ID <- MDV <- NULL
  if (vpc) {
    warning("vpc is not working for nlmixr2 currently; disabling vpc")
  }
  if (missing(fit)) {
    return(NULL)
  }

  if (inherits(fit, "nlmixr2FitData")) {
  } else {
    stop("unsupported 'fit' object", call.=FALSE)
  }
  config <- "standing"
  directory <- ""
  dv <- "DV"
  cats <- if (missing(cats)) "" else cats
  conts <- if (missing(conts)) "" else conts
  occ <- ""
  strats <- if (missing(strats)) "" else strats
  dvid <- if (missing(dvid)) ifelse(any(names(fit) == "CMT"), "CMT", "") else dvid
  endpoint <- if (missing(endpoint)) NULL else endpoint
  if (missing(settings)) settings <- pmx_settings()
  if (!inherits(settings, "pmxSettingsClass")) {
    settings <- pmx_settings()
  }

  if (!("NPDE" %in% names(fit))) {
    fitN <- try(nlmixr2::addNpde(fit), silent=TRUE)
    if (!inherits(fitN, "try-error")) {
      fit <- fitN
    }
  }

  finegrid <- try(invisible(nlmixr2::augPred(fit)), silent = TRUE)
  if (inherits(finegrid, "try-error")) {
    message(paste0("Error creating finegrid: ", attr(finegrid, "condition")$message))
    finegrid <- NULL
  } else {
    class(finegrid$values) <- "double"
    finegrid <- dcast(setDT(finegrid), id + time + Endpoint ~ ind, value.var = "values", fun.aggregate=mean)
    setnames(finegrid,
             c("id", "time", "Endpoint","Population", "Individual", "Observed"),
             c("ID", "TIME", ifelse(dvid == "", "DVID", ""), "PRED", "IPRED", "DV")
             )
  }


  sim <- NULL
  if (vpc) {
    sim_data <- try(nlmixr2::vpcSim(fit), silent = TRUE)
    sim_data <- setDT(sim_data)
    setnames(sim_data, "sim", "DV")
    if (inherits(sim_data, "try-error")) {
      message(paste0("Error creating VPC: ", attr(sim_data, "condition")$message))
      sim <- NULL
    } else {
      if (any(names(sim_data) == "rxLambda")) {
        sim_data[, c("rxLambda", "rxYj", "rxLow", "rxHi") := NULL]
      }
      sim <- pmx_sim(data = sim_data, idv = "time", irun = "sim.id")
    }
  }


  domega <- diag(fit$omega)
  omega <- data.table(
    EFFECT = sub("[.]?(eta|bsv)[.]?", "", names(domega)),
    OMEGA = sqrt(as.vector(domega))
  )

  ## nlmixr ID datasets are actually factors.  Merging by ID with the
  ## original dataset is risky.
  FIT <- as.data.frame(fit)
  FIT$ID <- paste(FIT$ID)
  FIT <- data.table(FIT)

  if (!is.null(endpoint)) {
    if (!is.null(dvid) && dvid %in% names(FIT)) {
      rr <- dvid
      FIT <- FIT[get(rr) == endpoint$code]
      if (!nrow(FIT)) {
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
    if (!is.null(dvid) && dvid %in% names(FIT)) {
      rr <- dvid
      ends <- unique(FIT[, get(rr)])
      if (length(ends) > 1) {
        msg <- sprintf("Observation data contains multiple endpoints %s. \n ", paste(ends, collapse = " ; "))
        msg <- paste(msg, "Please select a single endpoint to continue.")

        stop(msg)
      }
    }
  }
  # use the fit DV because it simulates censoring
  # $fitMergeInner prefers the DV from the fit, so preserve it here
  input <- fit$fitMergeInner
  # if it hasn't been implemented use dataMergeInner instead
  if (is.null(input)) input <- fit$dataMergeInner
  input <- as.data.table(input)
  for (v in conts) {
    if (v != "") {
      class(input[[v]]) <- "double"
    }
  }
  eta <- copy(input)
  ## The eta parameters do not have to be named eta
  measures <- names(fit$eta)[-1]
  if (length(measures) == 0) {
    message("NO random effect found")
  }
  eta_trans <- setNames(measures, sub("[.]?(eta|bsv)[.]?", "", measures))
  ## Not necessary these are already numeric... Perhaps a good practice?
  eta[, (measures) := lapply(.SD, as.numeric), .SDcols = measures]
  eta <- melt(eta, measure = measures)
  setnames(eta, c("value", "variable"), c("VALUE", "EFFECT"))
  eta[, EFFECT := sub("[.]?(eta|bsv)[.]?", "", EFFECT)]

  ## Now get estimates

  ### PARAM    VALUE      SE    RSE    PVALUE
  pars <- fit$parFixedDf
  ini_eta <- fit$iniDf
  ini_theta <- ini_eta[is.na(ini_eta$neta1), ]
  ini_err <- ini_theta[!is.na(ini_theta$err), ]
  ini_theta <- ini_theta[is.na(ini_theta$err), ]
  ini_eta <- ini_eta[!is.na(ini_eta$neta1), ]
  if (any(names(pars) == "Est.")) {
    est <- rbind(data.frame(PARAM=row.names(pars), VALUE=pars$`Est.`, SE=NA_real_, RSE=NA_real_),
                 data.frame(PARAM=ini_eta$name, VALUE=ini_eta$est, SE= -Inf, RSE= -Inf),
                 data.frame(PARAM="OBJ", VALUE=fit$objf, SE= -Inf, RSE= -Inf))

  } else {
    est <- rbind(data.frame(PARAM=row.names(pars), VALUE=pars$Estimate, SE=pars$SE, RSE=pars$`%RSE`),
                 data.frame(PARAM=ini_eta$name, VALUE=ini_eta$est, SE= -Inf, RSE= -Inf),
                 data.frame(PARAM="OBJ", VALUE=fit$objf, SE= -Inf, RSE= -Inf))
  }
  row.names(est) <- NULL

  param_regs <- c(theta=paste0("(", paste(gsub("[.]", "[.]", ini_theta$name), collapse="|"), ")"),
                  eta=paste0("(", paste(gsub("[.]", "[.]", ini_eta$name), collapse="|"), ")"),
                  err=paste0("(", paste(gsub("[.]", "[.]", ini_err$name), collapse="|"), ")"))

  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", config))
  pconfig <- yaml.load_file(pfile)
  w <- which(tolower(names(input)) == "npd")
  doNpd <- FALSE
  if (length(w) == 1) {
    names(input)[w] <- "NPD"
    doNpd <- TRUE
  }
  w <- which(tolower(names(input)) == "epred")
  if (length(w) == 1) {
    names(input)[w] <- "EPRED"
    if (doNpd) {
      pfile <- file.path(plot_dir, "npd.ppmx")
      pconfig <- c(pconfig, yaml.load_file(pfile))
    }
  }
  w <- which(tolower(names(input)) == "cwres")
  doCwres <- FALSE
  if (length(w) == 1) {
    names(input)[w] <- "CWRES"
    doCwres <- TRUE
  }
  w <- which(tolower(names(input)) == "cpred")
  if (length(w) == 1) {
    names(input)[w] <- "CPRED"
    if (doCwres) {
      pfile <- file.path(plot_dir, "cwres.ppmx")
      pconfig <- c(pconfig, yaml.load_file(pfile))
    }
  }
  config <- list(
    sys = "nlmixr",
    plots = pconfig,
    omega = omega,
    finegrid = finegrid,
    eta = eta,
    parameters=est,
    hasNpd=doNpd,
    eta_trans=eta_trans,
    param_regs=param_regs
  )
  class(config) <- "pmxConfig"

  bloq <- NULL
  sim_blq <- FALSE

  pmxClass$new(directory, input, dv, config, dvid, cats, conts, occ, strats, settings, endpoint, sim, bloq, sim_blq)
}

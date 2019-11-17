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

pmx_nlmixr <- function(fit, dvid, conts, cats, strats, endpoint, settings, vpc = TRUE) {
  EFFECT <- EVID <- ID <- MDV <- NULL
  if (missing(fit)) {
    return(NULL)
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


  if (!"NPDE" %in% names(fit)) try(fit <- nlmixr::addNpde(fit), silent = TRUE)

  finegrid <- try(invisible(nlmixr::augPred(fit)), silent = TRUE)
  if (inherits(finegrid, "try-error")) {
    finegrid <- NULL
  } else {
    finegrid <- dcast(setDT(finegrid), id + time ~ ind, value.var = "values")
    setnames(
      finegrid, c("id", "time", "Population", "Individual", "Observed"),
      c("ID", "TIME", "PRED", "IPRED", "DV")
    )
  }

  sim <- NULL
  if (vpc) {
    sim_data <- try(invisible(nlmixr::vpc(fit)$rxsim), silent = TRUE)
    if (inherits(sim_data, "try-error")) {
      sim <- NULL
    } else {
      sim_data <- setDT(sim_data)
      setnames(sim_data, "dv", "DV")
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
  obs <- as.data.table(nlmixr::getData(fit))
  ## obs <- obs[!(EVID == 1 & MDV == 1)]
  if (any(names(obs) == "EVID")) {
    obs <- obs[EVID == 0 || EVID == 2]
  } else if (any(names(obs) == "MDV")) {
    obs <- obs[MDV == 0]
  }
  if (any(names(obs) == "ID")) {
    obs$ID <- paste(obs$ID)
  }
  ## Merge with DV too
  no_cols <- setdiff(intersect(names(FIT), names(obs)), c("ID", "TIME"))
  obs[, (no_cols) := NULL]
  uID <- unique(FIT$ID)
  obs <- subset(obs, ID %in% uID)
  obs$ID <- factor(obs$ID, levels = levels(fit$ID))
  FIT$ID <- factor(FIT$ID, levels = levels(fit$ID))
  input <- merge(obs, FIT, by = c("ID", "TIME"))
  if (length(input$ID) == 0L) {
    stop("Cannot merge nlmixr fit with observation dataset")
  }
  eta <- copy(input)
  ## The eta parameters do not have to be named eta
  measures <- names(fit$eta)[-1]
  if (length(measures) == 0) {
    message("NO random effect found")
  }
  ## Not necessary these are already numeric... Perhaps a good practice?
  eta[, (measures) := lapply(.SD, as.numeric), .SDcols = measures]
  eta <- melt(eta, measure = measures)
  setnames(eta, c("value", "variable"), c("VALUE", "EFFECT"))
  eta[, EFFECT := sub("[.]?(eta|bsv)[.]?", "", EFFECT)]

  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", config))
  pconfig <- yaml.load_file(pfile)
  config <- list(
    sys = "nlmixr",
    plots = pconfig,
    omega = omega,
    finegrid = finegrid,
    eta = eta
  )
  class(config) <- "pmxConfig"


  bloq <- NULL

  pmxClass$new(directory, input, dv, config, dvid, cats, conts, occ, strats, settings, endpoint, sim, bloq)
}

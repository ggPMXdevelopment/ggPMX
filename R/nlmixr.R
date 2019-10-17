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

#' @return \code{pmxClass} controller object.
#' @export

pmx_nlmixr <- function(fit, dvid, conts, cats, strats, endpoint, settings) {
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
  dvid <- if (missing(dvid)) ifelse(any(names(fit) == "CMT") , "CMT", "") else dvid
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



  sim_data <- try(invisible(nlmixr::vpc(fit)$rxsim), silent = TRUE)
  if (inherits(sim_data, "try-error")) {
    sim <- NULL
  } else {
    sim_data <- setDT(sim_data)
    print(sim_data);
    setnames(sim_data, "dv", "DV")
    sim <- pmx_sim(data = sim_data, idv = "time", irun = "sim.id")
  }


  domega <- diag(fit$omega)
  omega <- data.table(
    EFFECT = sub("eta.", "", names(domega)),
    OMEGA = sqrt(as.vector(domega))
  )




  FIT <- as.data.table(fit)
  FIT[, ID := as.integer(ID)]



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
  obs <- as.data.table(nlme::getData(fit))
  obs <- obs[!(EVID == 1 & MDV == 1)]

  no_cols <- setdiff(intersect(names(FIT), names(obs)), c("ID", "TIME", "DV"))
  input <- merge(obs, FIT[, !(no_cols), with = FALSE], by = c("ID", "TIME", "DV"))

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
  eta[, EFFECT := sub("[.]?eta[.]?", "", EFFECT)]

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


## ctr <- theophylline

## ctr  <- pmx_nlmixr(fit)


#' Title ggPMX nlimixr ctor
#'
#' @param ... 
#'
#' @return ctor
#' @export
#'
pmx_nlmixr <- function(fit,config="standing", ...){
  
  if (missing(fit)) return(NULL)
  if (!"NPDE" %in% names(fit)) fit <- addNpde(fit)
  
  finegrid <- nlmixr::augPred(fit)
  finegrid <- dcast(setDT(finegrid), id+time~ind,value.var="values")
  setnames(finegrid,c("id","time","Population" ,"Individual" ,"Observed"),
           c("ID","TIME","PRED","IPRED","DV"))
  
  
  sim_data <- try(invisible(setDT(nlmixr::vpc(fit)$rxsim)),silent=TRUE)
  if(inherits(sim_data,"try-error")) {
    sim <- NULL 
  }else{
    setnames(sim_data,"dv","DV")
    sim <- pmx_sim(data=sim_data,idv="time",irun="sim.id")
  }

  
  domega <- diag(fit$omega)
  omega <- data.table(
    EFFECT=sub("eta.","",names(domega)),
    OMEGA=as.vector(domega))
  

  
  
  FIT <- as.data.table(fit)
  
  eta <- copy(FIT)
  measures <- grep("^eta.*", names(eta))
  if (length(measures) == 0) {
    message("NO random effect found")
  }
  eta[, (measures) := lapply(.SD, as.numeric), .SDcols = measures]
  eta <- melt(eta, measure = measures)
  setnames(eta, c("value","variable"), c("VALUE","EFFECT"))
  eta[,EFFECT:=sub("eta.","",EFFECT)]
  
  
  directory <- ""
  input <- FIT[,ID:=as.integer(ID)]
  dv <- "DV"
  cats <- ""
  endpoint <- NULL
  conts <- ""
  occ <- ""
  strats <- ""
  dvid <- ""
  
  
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", config))
  pconfig <- yaml.load_file(pfile)
  config <- list(
    sys   = "nlmixr",
    plots = pconfig, 
    omega = omega, 
    finegrid = finegrid,
    eta=eta
  )
  class(config) <- "pmxConfig"
  
  
  settings <- pmx_settings()
  
  bloq <- NULL
  
  
  
  pmxClass$new(directory, input, dv, config, dvid, cats, conts, occ, strats, settings, endpoint, sim, bloq)
}


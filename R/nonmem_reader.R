#' Creates pmx controller from  NONMEM output
#'
#' @param runno 
#' @param table_suffix 
#' @param sim.suffix 
#' @param cwres.suffix 
#' @param directory 
#' @param quiet 
#' @param table_names 
#' @param cwres.name 
#' @param dvid 
#' @param conts 
#' @param cats 
#' @param strats 
#' @param endpoint 
#' @param settings 
#' @param vpc 
#' @param pred 
#' @param bloq 
#' @param dv 
#' @param obs 
#' @param time 
#'
#' @return
#' @export
#'
#' @examples ##add TIME!
pmx_nm <-function(runno = NULL,
                      table_suffix="",
                      sim.suffix="sim",
                      cwres.suffix="",
                      directory=".",
                      quiet=TRUE,
                      table_names=c("sdtab","mutab","patab","catab",
                                    "cotab","mytab","extra","xptab","cwtab"),
                      cwres.name=c("cwtab"),
                      dvid = "DVID", 
                      dv = "DV", 
                      conts, 
                      cats, 
                      strats="", 
                      endpoint, 
                      settings = pmx_settings(), 
                      vpc = FALSE, 
                      pred = "PRED", 
                      bloq = NULL, 
                      obs = TRUE, 
                      time = "TIME") {
  
  
  

  
  ##
  config <- "standing"
  occ <- ""
  finegrid <- NULL

  ##
  ##options(warn=-1) # suppress warnings
  ## make table lists
  match.pos <- match(cwres.name,table_names) #shows position of cwtab
  if (!is.na(match.pos)) table_names <- table_names[-match.pos] #remove cwtab from table_names if its there
  
  ## Create the table file names to process
  myfun <- function(x,directory,runno,cwres.suffix,sim.suffix,table_suffix) {
    filename <- paste0(x,runno,cwres.suffix,sim.suffix,table_suffix)
    file.path(directory, filename)
  }
  
  cotab.file <- myfun("cotab",directory,runno,cwres.suffix="",sim.suffix="",table_suffix)
  
  catab.file <- myfun("catab",directory,runno,cwres.suffix="",sim.suffix="",table_suffix)
  
  sdtab.file <- myfun("sdtab",directory,runno,cwres.suffix="",sim.suffix="",table_suffix)
  
  tab.files   <- sapply(table_names,myfun,directory,runno,cwres.suffix="",sim.suffix="",table_suffix)
  
  cwres.files <- sapply(cwres.name,myfun,directory,runno,cwres.suffix,sim.suffix="",table_suffix)
  
  sim.files   <- sapply(table_names,myfun,directory,runno,cwres.suffix="",sim.suffix,table_suffix)
  
  cwres.sim.files <- sapply(cwres.name,myfun,directory,runno,cwres.suffix,sim.suffix,table_suffix)
  
  tab.files <- c(tab.files,cwres.files)
  sim.files <- c(sim.files,cwres.sim.files)
  
  ## Read the table files.
  cat("\nLooking for NONMEM table files.\n")
  
  tmp_lst <- read.nm.tables(table.files=tab.files,
                            quiet=quiet)
  
  if (missing(tmp_lst)) tmp_lst <- NULL
  
  if (is.null(tmp_lst)) {
    stop("No files found! Controller could not be created")
  }
    
  
  tmp    <- tmp_lst[[1]] #containing the merged dataset
  ca_tmp <- tmp_lst[[2]] #containing cats header names
  co_tmp <- tmp_lst[[3]] #containing conts header names
  sd_tmp <- tmp_lst[[4]] #containing conts header names
  
  
  #tmp$TIM2 <- log(tmp$TIME) #for testing purposes
  #tmp$LNDV <- log(tmp$DV) #for testing purposes
  
  ## Extracting covariates from catab/cotab files if they're not specified
  if(missing(cats) & !is.null(sd_tmp)) {
    
    if(is.null(ca_tmp)){
      cat("\nNo catab file found\n")
      cats <- ""
    }
    
    if(missing(cats)) {
      cats <- ca_tmp[which(is.na(sd_tmp[match(ca_tmp,sd_tmp)]))] ## Change to setdiff cuntion
      cat(paste0("\n",cats, " was extracted from catab file\n"))
    }
    
  } else {
    if (missing(cats)) cats <- "" else cats
  }
  
  
  if(missing(conts) & !is.null(sd_tmp)) {
    
    if(is.null(co_tmp)){
      cat("\nNo cotab file found\n")
      conts <- ""
    }
    
    if(missing(conts)) {
      conts <- co_tmp[which(is.na(sd_tmp[match(co_tmp,sd_tmp)]))]
      cat(paste0("\n",conts, " was extracted from cotab file\n"))
    }
    
  } else {
    if (missing(conts)) conts <- "" else conts
  }
  
  if(is.null(sd_tmp) & (!is.null(ca_tmp) | !is.null(co_tmp))){
    cat("\nsdtab is needed to specificy covariates automatically\n")
  }

  
  ## Rename variables to ggPMX nomenclature
  
  ## Rename variables which cannot be specified in the pmx_nm() function, which rely on convetions
  tmp$IPRED <- tmp[,grep("IPRED", names(tmp))]
  tmp$NPDE <- tmp[,grep("NPD", names(tmp))]
  tmp$IWRES <- tmp[,grep("IWRES", names(tmp))]
  
  ## Rename variables which can be specified in the pmx_nm() function
  # x = ggPMX nomenclature name
  # y = user defined name
  # data = dataset
  naming_fun <- function(x,y,data) { 
    if(x != y) {
      data[[x]] <- NULL
      if(any(y == names(data))){
        data[[x]] <- data[[y]]
        cat(paste(y,"has been specified as",x,"\n"))
        return(data)
      } else {
        stop(paste(y,"not found in dataset! Please check naming of",x))
        return(data)
      }
    } else {
      return(data)
    }
  }
  
  tmp <- naming_fun("DV",dv,tmp)
  tmp <- naming_fun("DVID",dvid,tmp)
  tmp <- naming_fun("TIME",time,tmp)
  tmp <- naming_fun("PRED",pred,tmp)
  
  
  ## remove non-observation rows if obs = TRUE
  MDV <- c()
  if(obs){
    if(any("MDV"==names(tmp))){
      tmp <- dplyr::filter(tmp,MDV==0)   
    } else {
      
        if(any("EVID"==names(tmp))) {
          tmp$MDV <- tmp[,grep("EVID", names(tmp))]
          tmp <- dplyr::filter(tmp,MDV==0) 
        } else {
          
          warning('\nMDV or EVID data item not listed in header, 
              Could not remove dose events!\n')
        }
      
    }
  }
  
  ###generate input variable
  input <- as.data.table(tmp)
  input_names <- names(input)
  
  ###endpoint handling
  endpoint <- if (missing(endpoint)) NULL else endpoint
  
  if(!is.null(endpoint)) {
    
    if(!(endpoint %in% unique(input$DVID))) {
      warning("Endpoint value does not correspond to", dvid ,"values!\n")
    } else {
      input <- dplyr::filter(input,DVID==endpoint)
    }
    
  }
  
  
  ## Generation of eta data.table
  eta <- input
  measures <- input_names[grep("ETA", input_names)]
  eta <- melt(eta, measure = measures)
  setnames(eta, c("value", "variable"), c("VALUE", "EFFECT"))
  eta <- as.data.table(eta)
  
  
  ## Parse parameters from .ext. file using read_nmext() function
  ext_file <- list.files(path = directory, pattern = "\\.ext$")
  
  if(length(ext_file) != 1) {
  if(length(ext_file) == 0) {
     warning("There is no .ext file in the directory\n")
  } else {
     warning(paste("There are multiple .ext files in the directory\n The current file is now being used:", ext_file[1]))
     ext_file <- ext_file[1]
   }
  }
  
  parameters <- read_nmext(file = ext_file, project = directory, run = "")
  
  momega <- parameters$omega
  domega <- as.double(diag(momega))
  names(domega) <- input_names[grepl("ETA", input_names)]
  omega <- data.table(
    EFFECT = sub("[.]?(eta|bsv)[.]?", "", names(domega)),
    OMEGA = sqrt(as.vector(domega))
  )
  
  
  ## Add code to restructure Parameters to a nicer output
  
  ##placeholder of handling of simulations for VPC, however might be already done by xpose_functions
  sim <- NULL
  
  
  ## error handling for simulations!
  cat("\nLooking for NONMEM simulation table files.\n")
  gosim <- TRUE
  simct <- FALSE
  
  ## check if there are any simulation files
  for(i in 1:length(sim.files)) {
    if (is.readable.file(sim.files[i]))  {
      simct <- TRUE
    }
  }
  
  if (simct){
    for(i in 1:length(tab.files)) {
      if ((is.readable.file(tab.files[i])) && (!is.readable.file(sim.files[i])))  {
        err.mess <- paste(sim.files[i],"not found!")
        gosim <- FALSE
        break
      }
    }
  } else {
    gosim <- FALSE
  }
  
  
  if (gosim==FALSE) {
    
    if (!simct) {
      #cat("  Files are either not present or not named correctly\n")
      #cat("  (e.g. sdtab1a instead of sdtab1sim)\n")
    } else {
      cat("  There is not the same number of normal and \n")
      cat("  simulation table files for the current run number:\n")
      cat(paste("  ",err.mess,"\n",sep=""))
    }
    cat("No simulated table files read.\n\n")
  }
  
  if (gosim==TRUE) {
    simtmp <- read.nm.tables(sim.files,
                             #runno,
                             #table_suffix=paste(sim.suffix,table_suffix,sep=""),
                             #cwres.suffix=paste(sim.suffix,cwres.suffix,sep=""),
                             quiet=quiet)
    
    if(!is.null(tmp)) {
      simtmp <- simtmp
      cat("Simulation table files read.\n")
    } else {
      cat("There was a problem reading the simulation tables!\n")
      cat("Simulation tables not read!\n")
      return(NULL)
    }
    
  }
  
  
  ##Generater the controller
  
  if (!inherits(settings, "pmxSettingsClass")) {
    settings <- pmx_settings()
  }
  
  assert_that(inherits(bloq, "pmxBLOQClass") || is.null(bloq))
  
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", config))
  pconfig <- yaml.load_file(pfile)
  config <- list(
    sys = "nm",
    plots = pconfig,
    omega = omega,
    finegrid = finegrid,
    eta = eta,
    bloq = bloq,
    parameters = parameters
  )
  class(config) <- "pmxConfig"
  

  
  pmxClass$new(directory, input, dv, config, dvid, cats, conts, occ, strats, settings, endpoint, sim, bloq, time) ##add TIME!
  
}

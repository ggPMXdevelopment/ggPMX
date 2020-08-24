#load is.readable.file
#load read_nm_tabl3
#load read.nm.tables

#dependent on readr function

pmx_nm <-function(runno = NULL,
                      tab.suffix="",
                      sim.suffix="sim",
                      cwres.suffix="",
                      directory=".",
                      quiet=TRUE,
                      table.names=c("sdtab","mutab","patab","catab",
                                    "cotab","mytab","extra","xptab","cwtab"),
                      cwres.name=c("cwtab"),
                      mod.prefix="run",
                      mod.suffix=".mod",
                      phi.suffix=".phi",
                      phi.file=NULL,
                      ##vpc.name="vpctab",
                      nm7=NULL,  # T/F if table files are for NM7, NULL for undefined
                      dvid, conts, cats, strats, endpoint, settings, vpc = FALSE, pred = "PRED", bloq) {
  
  
  

  
  ##handling like pmx_nlmixr, however might not be required(?)
  EFFECT <- EVID <- ID <- MDV <- NULL
  runno <- if (missing(runno)) NULL else runno
  config <- "standing"
  dv <- "DV"
  cats <- if (missing(cats)) "" else cats #maybe parse out from files
  conts <- if (missing(conts)) "" else conts #maybe parse out from files
  occ <- ""
  strats <- if (missing(strats)) "" else strats
  if (missing(settings)) settings <- pmx_settings()
  if (!inherits(settings, "pmxSettingsClass")) {
    settings <- pmx_settings()
  }
  
  finegrid <- NULL #
  

  ##
  ##options(warn=-1) # suppress warnings
  ## make table lists
  match.pos <- match(cwres.name,table.names) #shows position of cwtab
  if (!is.na(match.pos)) table.names <- table.names[-match.pos] #remove cwtab from table_names if its there
  
  ## Create the table file names to process
  myfun <- function(x,directory,runno,cwres.suffix,sim.suffix,tab.suffix) {
    filename <- paste0(x,runno,cwres.suffix,sim.suffix,tab.suffix)
    file.path(directory, filename)
  }
  
  tab.files   <- sapply(table.names,myfun,directory,runno,cwres.suffix="",sim.suffix="",tab.suffix)
  
  cwres.files <- sapply(cwres.name,myfun,directory,runno,cwres.suffix,sim.suffix="",tab.suffix)
  
  sim.files   <- sapply(table.names,myfun,directory,runno,cwres.suffix="",sim.suffix,tab.suffix)
  
  cwres.sim.files <- sapply(cwres.name,myfun,directory,runno,cwres.suffix,sim.suffix,tab.suffix)
  
  tab.files <- c(tab.files,cwres.files)
  sim.files <- c(sim.files,cwres.sim.files)
  
  ## Read the table files.
  cat("\nLooking for NONMEM table files.\n")
  tmp <- read.nm.tables(table.files=tab.files,
                        quiet=quiet)
  
  ## Fail if we can't find any.
  if(is.null(tmp)) {
    cat("Table files not read!\n")
    return(NULL)
  }
  
  ## check if NM.version is > 6
  if(is.null(nm7)){
    if(any(!is.na(match(c("IPRED","IWRES"),names(tmp))))){
      nm7 <- T
    } else {
      nm7 <- F
    }
  }
  
  
  ##placeholder of handling of simulations, however might be already done by xpose_functions
  sim <- NULL
  #sim_data <- NULL
  #setnames(sim_data, "dv", "DV")
  #if(vpc) {}
  #sim <- pmx_sim(data = sim_data, idv = "time", irun = "sim.id")
  
  
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
                             #tab.suffix=paste(sim.suffix,tab.suffix,sep=""),
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
  
  #RENAME the output to work on ggPMX (Maybe add warning if NAMES are not recognized or one of the Names Missing or not recognized)
  
  #evtl add, if !EVID then (so if evid FALSE, execute the code)
  if("EVID" %in% names(tmp)) {
    tmp <- tmp[which(tmp$EVID == 0),] #remove EVID != 0
    tmp$EVID <- NULL #Remove EVID columng
  }
  
  tmp_names_org <- names(tmp)
  
  ##handling of DVID, so user can specificy
  dvid <- if (missing(dvid)) "DVID" else dvid
  if(dvid != "DVID") { #error if specified column does not matach dataset
    if(!(dvid %in% tmp_names_org)) {
      stop(paste(dvid, "column not found in dataset!\n"))
    } 
  }
  
  if("DVID" %in% tmp_names_org & dvid != "DVID") {
    cat("DVID already present in the dataset!\n")
    cat(paste("DVID will be replaced by values of ",dvid,"!\n"))
    tmp <- tmp[,-grep("DVID", tmp_names_org)]
    tmp_names_org <- names(tmp)
    tmp_names_org[grep(dvid, tmp_names_org)] <- "DVID"
  }
  
  tmp_names_new <- tmp_names_org
  tmp_names_new[grep("IPRED", tmp_names_org)] <- "IPRED"
  tmp_names_new[grep("NPD", tmp_names_org)] <- "NPDE"
  tmp_names_new[grep("IWRES", tmp_names_org)] <- "IWRES"
  
  # handling of PRED because NONMEM can output multiple types of PRED
  ##User has to choose it, the rest of the preds is removed is removed, but notify the user.
  
  ##
  
  if(!grepl("PRED", pred, fixed = TRUE) | length(pred) != 1) {
    stop("PRED variable has to contain <PRED> (e.g. PRED, CPRED, CPREDI etc.) Check naming of PRED.\n  Only one PRED variable can be specified.")
  }
  
  if(grepl("IPRED", pred)) {
    stop("PRED might be wrongly specified as IPRED!\n")
  }
  
  if(!(pred %in% tmp_names_new)) {
    stop(paste(pred,"not found in dataset! PRED might be wrongly specified\n"))
  }
  
  to_keep_vec <- which(pred == tmp_names_new | "IPRED" == tmp_names_new) ##keep these ones
  pred_vec <- grep("PRED", tmp_names_new) ##get positions of all variables including "PRED"
  
  to_remove_vec <- pred_vec[!(pred_vec %in% to_keep_vec)] ##these PREDs have to be removed
  to_remove_names <- tmp_names_new[to_remove_vec]
  
  cat(paste(pred, "was chosen as PRED variable\n"))
  
  if (pred != "PRED") {
    tmp_names_new[pred == tmp_names_org] <- "PRED" #rename to "PRED" for ggPMX
    cat(paste(pred, "column was renamed to PRED \n"))
  }
  
  names(tmp) <- tmp_names_new
  
  if(length(to_remove_vec) != 0L) {
    tmp <- tmp[,-to_remove_vec]
  }
  
  input <- as.data.table(tmp)
  input_names <- names(input)
  
  
  ###endpoint handling
  endpoint <- if (missing(endpoint)) NULL else endpoint
  
  if(!is.null(endpoint)) {
    
    if(!(endpoint %in% unique(input$DVID))) {
      warning("Endpoint value does not correspond to", dvid ,"values!\n")
    } else {
      input <- input[which(input$DVID == endpoint),]
    }
    
  }
  
  
  ###generation of eta
  eta <- input
  measures <- input_names[grep("ETA", input_names)]
  eta <- melt(eta, measure = measures)
  setnames(eta, c("value", "variable"), c("VALUE", "EFFECT"))
  
  eta <- as.data.table(eta)
  
  
  ###Parse .lst file to retrieve OMEGA matrix
  #specify header for omega
  omega <- NULL
  
  #maybe add error handling if omega is not found
  
  omega_header <- "OMEGA - COV MATRIX FOR RANDOM EFFECTS" #don't know if best way to handle it
  sigma_header <- "SIGMA - COV MATRIX FOR RANDOM EFFECTS"
  
  lst_file <- list.files(path = directory, pattern = "\\.lst$")
  
  if(length(lst_file) != 1) {
    if(length(lst_file) == 0) {
      warning("There is no .lst file in the directory\n")
    } else {
      warning(paste("There are multiple .lst files in the directory\n The current file is now being used:", lst_file[1]))
      lst_file <- lst_file[1]
    }
  }
  
  lst_path <- file.path(directory, lst_file)
  lst <- read_lines(lst_path, n_max = -1L)
  
  top <- grep(omega_header, lst)
  bottom <- grep(sigma_header, lst)

  lst <- lst[(top+3):(bottom-4)]
  lst <- lst[-2]
  header_vec <- input_names[grep("ETA", input_names)]
  tbble_lst <- read_table(lst, col_names = c("X1",header_vec))
  tbble_lst <- tbble_lst[-1,-1]
  names_tbble <- names(tbble_lst) #probably not necessary and could use precified header_vec
  tbble_lst <- suppressWarnings(tbble_lst[!is.na(tbble_lst),]) #notoptimal to surpressWarning, maybe need optimization
  matrix_lst <- as.matrix(tbble_lst)
  domega <- as.double(diag(matrix_lst))
  names(domega) <- names_tbble
  
  omega <- data.table(
    EFFECT = sub("[.]?(eta|bsv)[.]?", "", names(domega)),
    OMEGA = sqrt(as.vector(domega))
  )
  
  if (missing(bloq)) bloq <- NULL
  assert_that(inherits(bloq, "pmxBLOQClass") || is.null(bloq))
  
  ##Generater the controller
  
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", config))
  pconfig <- yaml.load_file(pfile)
  config <- list(
    sys = "nm",
    plots = pconfig,
    omega = omega,
    finegrid = finegrid,
    eta = eta,
    bloq = bloq
  )
  class(config) <- "pmxConfig"
  

  
  pmxClass$new(directory, input, dv, config, dvid, cats, conts, occ, strats, settings, endpoint, sim, bloq)
  
}

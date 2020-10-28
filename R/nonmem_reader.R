# The ggPMX NONMEM reader (pmx_nm) is strongly based on NONMEM reading functions of the xpose package (v.0.4.11) (Thanks to Benjamin Guiastrennec)
# Dependent on read_nm_tables.R, read_nm_files.R, pmx_read_nm_model.R, list_nmtables.R, manual_import.R and utils_nonmem_reader.R
# The code was slightly adjusted to allow import without a model file (.lst/.ctl) (see alternative_import = TRUE)
# Furthermore, manual table import is possible without using "pmx_manual_nm_import()" as a user specified parameter in the function.
# Instead the user can specifiy manual tables in "table_names" (see tab_man_specified).
# Additionally, it is possible to include a simulation model file (e.g. simfile = simulation.ctl) to load post-hoc simulations, if no run number is specified.
# Reading the .ext files for parameters is done by using the read_extfile function (see read_extfile.R)

#' Creates pmx controller from NONMEM model outputs
#'
#' @param runno run number which is used for generating the model file name, or used for alternative import of NONMEM-output tables. 
#' @param table_suffix suffix of the output tables, standard is "" (no suffix).
#' @param prefix Prefix to be used to generate model file name. Used in combination with \code{runno} and \code{ext}.
#' @param directory directory of the model files.
#' @param table_names contains the names of the NONMEM-output tables e.g. "sdtab", "patab", "cotab", "catab".
#' @param simfile Useful if the simulation is peformed post-hoc and an additional simulation model file is generated e.g. "simulation.lst"; similar to "file" see above.
#' @param dvid \emph{[Optional]} \code{character} observation type parameter, mandatory in case of multiple endpoint (PKPD). Standard = "DVID"
#' @param conts \emph{[Optional]} \code{character} vector of continuous covariates (automatically detected if "cotab" is provided)
#' @param cats \emph{[Optional]} \code{character} vector of categorical covariates (automatically detected if "catab" is provided)
#' @param strats \emph{[Optional]} \code{character} extra stratification variables
#' @param endpoint \emph{[Optional]} \code{pmxEndpointClass} or \code{integer} or \code{charcater} default to NULL of the endpoint code. \code{\link{pmx_endpoint}}
#' @param settings \code{pmxSettingsClass} \code{\link{pmx_settings}} shared between all plots
#' @param vpc \code{logical} a boolean indiacting if vpc should be calculated, simulation tables are required for VPC generation (by default \code{TRUE})
#' @param pred \emph{[Optional]} \code{character} specifing variable name of the population prediction (standard ggPMX nomenclautre  = "PRED")
#' @param bloq \code{pmxBLOQClass} default to NULL. \code{\link{pmx_bloq}} specify bloq, within controller: e.g. bloq=pmx_bloq(cens = "BLOQ_name", limit = "LIMIT_name")
#' @param dv \code{character} the name of measurable variable used in the input modelling file (standard ggPMX nomenclautre  = "DV")
#' @param obs \code{logical} if set to TRUE will filter dataset according to "MDV", default is FALSE
#' @param time \emph{[Optional]} \code{character} specifing variable name of time (standard ggPMX nomenclautre  = "TIME")
#' @param file A character vector of path to the files or a \code{nm_table_list} object created with \code{pmx_list_nm_tables}.
#' @param ext Extension to be used to generate model file name. Should be one of'.lst' (default), '.out', '.res', '.mod' or '.ctl' for NONMEM.
#' @param sim_suffix suffix of the simulation output tables, standard is "sim" (e.g. stdab1sim).
#' @param npde \emph{[Optional]} \code{character} specifing variable name of the normalized population predictor (standard ggPMX nomenclautre  = "NPDE")
#' @param iwres \emph{[Optional]} \code{character} specifing variable name of the individual weighted residuals (standard ggPMX nomenclautre  = "IWRES")
#' @param ipred \emph{[Optional]} \code{character} specifing variable name of the individual population prediction (standard ggPMX nomenclautre  = "IPRED")
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @author The ggPMX NONMEM reader (pmx_nm) is strongly based on NONMEM reading functions of the xpose package (v.0.4.11) (Thanks to Benjamin Guiastrennec) 
#' To avoid conflicts with the xpose package, the necessary xpose-based functions have been renamed with a "pmx_" prefix. If the user wants to use individual functions
#' e.g. "read_nm_tables" please use the xpose-package
#'
#' @return \code{pmxClass} controller object.
#' @export
#'
#' @example inst/examples/pmx_nm.R
pmx_nm <-function(file = NULL, directory=".", runno = NULL, ext =".lst", table_suffix="", sim_suffix="sim", simfile = NULL, prefix = "run", 
                  table_names=c("sdtab","mutab","patab","catab","cotab","mytab","extra","xptab","cwtab"), dvid = "DVID", 
                  pred = "PRED", time = "TIME", dv = "DV", conts, cats, npde, iwres, ipred, endpoint, strats="",  
                  settings = pmx_settings(), vpc = TRUE, bloq = NULL, obs = FALSE, quiet = FALSE) {
  
  ## Avoid error message in cmd check
  
  MDV <- EVID <- DVID <- PARAM <- NULL
  
  ## Configuration for ggPMX
  
    config <- "standing"
    occ <- "" # Not implemented yet
    finegrid <- NULL
    sim_blq <- FALSE
  
  ## Currently not set as user-defined parameters in ggPMX
    
    ignore = NULL
    manual_import = NULL
    simtab = NULL
    
  ## Read the table files.
    
    dir <- directory # rename directory to xpose definition
    table_vec <- c("sdtab","mutab","patab","catab","cotab","mytab","extra","xptab","cwtab") #standard table names
    
    # Check if table names were manually specified for individual loading
    
    tab_man_specified <- NULL
    if(!identical(table_vec,table_names)) { 
      tab_man_specified <- TRUE
    }
    
    # Check if runno file or table_names were specified
    
    if (is.null(runno) && is.null(file) && is.null(tab_man_specified)) { 
      stop('Argument `runno`, `file` or  `table_names` required.', call. = FALSE)
    }
      
    
    # Check extensions
    
    if (!is.null(runno)) {
      ext <- pmx_make_extension(ext)
      full_path <- pmx_file_path(dir, stringr::str_c(prefix, runno, ext))
    } else {
      ext <- pmx_get_extension(file)
      if(length(ext) == 0) { #this is different to the code in xpose to allow alternative_import without model file (.lst/.ctl)
        ext <- ".lst"
        file <- "nofile"
      }
      if (ext == '') stop('An extension should be provided in the `file` name.', call. = FALSE)
      full_path <- pmx_file_path(dir, file)
    }
    
    # Allow alternative import when model file (.lst/.ctl) is not found
    # Alternative import is independent of the model file (.lst/.ctl) and similar to xpose4
    
    alternative_import = FALSE
      if(!file.exists(full_path)){
        pmx_msg("Alternative import is used without model file",quiet)
        #warning("No model file was found, check naming of files\n")
        alternative_import = TRUE
        file = NULL
      }
    
    
    ## Alternative import without loading model files (.lst/.ctl) (e.g. similar to xpose4)
    
    if(alternative_import){
      software   <- 'nonmem'
      tab_list <- pmx_manual_nm_import(tab_names = table_names, tab_suffix = table_suffix, sim_suffix = sim_suffix)
      
      if(is.null(runno)){
        tbl_names <- file.path(dir,tab_list$tab_names)
        data <- tryCatch(pmx_read_nm_tables(file = tbl_names, dir = NULL, 
                                        quiet = quiet, simtab = simtab, user_mode = FALSE), 
                         error = function(e) {
                           warning(e$message, call. = FALSE)
                           return()
                         })
      } else {
        
        tbl_names <- pmx_list_nm_tables_manual(runno = runno, dir = dir, tab_list = tab_list)
        data <- tryCatch(pmx_read_nm_tables(file = tbl_names, dir = NULL, 
                                        quiet = quiet, simtab = simtab),
                         error = function(e) {
                           warning(e$message, call. = FALSE)
                           return()
                         })
      }
    }
    
    ## "Normal" import with loading model files(.lst/.ctl) (according to xpose)
    
    if(!alternative_import) {
      # List tables
      if (ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
        software   <- 'nonmem'
        model_code <- pmx_read_nm_model(file = basename(full_path), 
                                    dir  = dirname(full_path))
        
        if (is.null(manual_import)) {
          tbl_names <- pmx_list_nm_tables(model_code)
        } else {
          tbl_names <- pmx_list_nm_tables_manual(runno = runno, file = basename(full_path), 
                                             dir = dirname(full_path), tab_list = manual_import)
        }
      } else {
        stop('Model file currently not supported by pmx_read_nm_tables', call. = FALSE)
      }  
      
      
      # Import estimation tables
      if ('data' %in% ignore) {
        pmx_msg('Ignoring data import.', quiet)
        data <- NULL
      } else if (software == 'nonmem') {
        data <- tryCatch(pmx_read_nm_tables(file = tbl_names, dir = NULL, 
                                        quiet = quiet, simtab = simtab), 
                         error = function(e) {
                           warning(e$message, call. = FALSE)
                           return()
                         })
      } 
      
    }
    
    ## Store file variable for getting the .ext file (used later in read_extfile)
    
    file_tab <- file
    ext_tab <- ext
    
    ## Get model predictions as data.table
    
    tmp <- data$data[data$simtab == FALSE]
    tmp <- as.data.table(tmp)
    
    if(length(tmp) == 0){
      stop("Error, no data could be loaded")
    }
    
    ## Get covariates
    
    index <- as.data.table(data$index[[1]])
    
    if(missing(conts)){
      conts <- index$col[which(index$type == "contcov")]
      if(length(conts) == 0) conts <- ""
    }

    if(missing(cats)){
      cats <- index$col[which(index$type == "catcov")]
      if(length(cats) == 0) cats <- ""
    }


  ## Rename variables to ggPMX nomenclature
    
  # Two functions are used to rename variables: check_nam_fun and naming_fun,
  # main difference is that check_nam_fun uses patterns to recognize variables using grep, 
  # whereas naming_fun is looking for the exact match
  # probably this is possible to simplify for future versions...
  
  # x = ggPMX nomenclature name
  # y = user defined name
  # z = substring which is used to "grep" variable name 
  # data = dataset
  
    check_nam_fun <- function(x,z,data) {
      if(any(x == names(data))){
        return(data)
      } else if(length(names(data)[grep(z,names(data))]) == 1)  {
        y <- names(data)[grep(z,names(data))]
        data[[x]] <- data[[y]]
        pmx_msg(paste(y,"has been specified as",x),quiet)
        return(data)
      } else if (length(names(data)[grep(z,names(data))]) == 0) {
        y <- names(data)[grep(z,names(data))]
        pmx_msg(paste(z,"not found in dataset! Please check naming of",x),quiet)
        return(data)
      } else{
        y <- names(data)[grep(z,names(data))]
        stop(paste("Multiple variables found containing",z,":",paste(c(y), collapse = ", "),"please specifiy",z))
      }
    }
  
    if(missing(npde)) {
      tmp <- check_nam_fun("NPDE", "NPD",tmp)
    } else {
      tmp$NPDE <- tmp[[npde]]
    }
  
    if(missing(ipred)) {
      tmp <- check_nam_fun("IPRED", "IPRED",tmp)
    } else {
      tmp$IPRED <- tmp[[ipred]]
    }
  
    if(missing(iwres)) {
      tmp <- check_nam_fun("IWRES", "IWRES",tmp)
    } else {
      tmp$IWRES <- tmp[[iwres]]
    }
  
  # x = ggPMX nomenclature name
  # y = user defined name
  # data = dataset
  
    naming_fun <- function(x,y,data) { 
      if(x != y) {
        data[[x]] <- NULL
        if(any(y == names(data))){
          data[[x]] <- data[[y]]
          pmx_msg(paste(y,"has been specified as",x),quiet)
          return(data)
        } else {
          warning(paste(y,"not found in dataset! Please check naming of",x))
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

    
  ## Handling of Simulation files
    
    sim_tmp <- data$data[data$simtab == TRUE]
    
    #if sim model file (e.g. sim.lst/.ctl) is provided make a vpc automatically
    if(!is.null(simfile)){ 
      vpc <- TRUE
    }
    
    #if sim model file (e.g. sim.lst/.ctl) is provided load simfile according xpose 
    dt_sim <- NULL    
    if(vpc & !is.null(simfile)) {
    
      file <- simfile
  
    # Check extensions
    if (!is.null(runno)) {
      ext <- pmx_make_extension(ext)
      full_path <- pmx_file_path(dir, stringr::str_c(prefix, runno, ext))
    } else {
      ext <- pmx_get_extension(file)
      if (ext == '') stop('An extension should be provided in the `file` name.', call. = FALSE)
      full_path <- pmx_file_path(dir, file)
    }
  
    # List tables
    if (ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
      software   <- 'nonmem'
      model_code <- pmx_read_nm_model(file = basename(full_path), 
                                  dir  = dirname(full_path))
    
      if (is.null(manual_import)) {
        tbl_names <- pmx_list_nm_tables(model_code)
      } else {
        tbl_names <- pmx_list_nm_tables_manual(runno = runno, file = basename(full_path), 
                                           dir = dirname(full_path), tab_list = manual_import)
      }
    
    } else {
      stop('Model file currently not supported by xpose.', call. = FALSE)
    }  
  
    # Import estimation tables
    simdata <- tryCatch(pmx_read_nm_tables(file = tbl_names, dir = NULL, 
                                       quiet = quiet, simtab = simtab), 
                        error = function(e) {
                          warning(e$message, call. = FALSE)
                          return()
                        })
  
  
    dt_sim <- as.data.table(simdata$data)
  
    } else if(length(sim_tmp) != 0 & vpc) { 
      dt_sim <- as.data.table(sim_tmp)
    }
  
    
    # Try to generate REP column if its not provided in the dataset

  if(!is.null(dt_sim)){
    if(is.null(dt_sim$REP)){
      pmx_msg("No REP column found, try to generate REP column",quiet)
      x <- nrow(dt_sim)/nrow(tmp)
      if(nrow(dt_sim)%%nrow(tmp) == 0){
        x_vec <- seq(1:x)
        dt_sim$REP <- 0
        i <- 1
        j <- 1
        k <- nrow(tmp)
        for(i in 1:length(x_vec)){
          dt_sim$REP[j:k] <- x_vec[i]
          j <- j + nrow(tmp)
          k <- k + nrow(tmp)
          i <- i + 1
        }
        pmx_msg("REP column generated",quiet)
      } else {
        dt_sim <- NULL
        warning("No REP column found and REP column could not be generated, simulation data not loaded")
      }
    
    }
  
    # Format simulation table to create simulaiton object for pmx (see down below ## Generate the Controller)
    dt_sim <- dt_sim[,c("REP","ID","TIME","DV")]
    dt_sim$ID <- as.factor(dt_sim$ID)
  }
  

  ## remove missing rows if obs = TRUE (if obs = FALSE, missing values (MDV == 1) will be kept)
  
    if(obs){
      if(any("MDV"==names(tmp))){
       tmp <- dplyr::filter(tmp,MDV==0)   
      } else {
       warning('MDV item not listed in header, 
                could not remove missing values!\n')
      }
    }
  
  ## remove non-observation rows according to EVID
  
  if(any("EVID"==names(tmp))) {
        tmp <- dplyr::filter(tmp,EVID==0) 
  }
  
  
  ## Generate "input" variable
    
    input <- as.data.table(tmp)
    input_names <- names(input)
  
  ## Endpoint handling
    
    endpoint <- if (missing(endpoint)) NULL else endpoint
  
    if(!is.null(endpoint)) {
      if(!(endpoint %in% unique(input$DVID))) {
        warning("Endpoint value does not correspond to", dvid ,"values!\n")
      } else {
        input <- as.data.table(dplyr::filter(input,DVID==endpoint))
      }
    }
  
    
  ## Generation of eta data.table
    if(!length(input_names[grep("ETA", input_names)]) == 0){
      eta <- input
      measures <- input_names[grep("ETA", input_names)]
      eta <- melt(eta, measure = measures)
      setnames(eta, c("value", "variable"), c("VALUE", "EFFECT"))
      eta <- as.data.table(eta)
    } else {
      eta <- NULL
    }

  ## Parse parameters from .ext. file using read_extfile() function
    
    # Check if unqiue .ext file can be recognized accoridng to model file or runno
    
    ext_file_exist_by_runno <- file.exists(file.path(directory,paste0(prefix,runno,".ext")))
    ext_file_exist_by_file  <- file.exists(file.path(directory,paste0(gsub(ext_tab, "", file_tab),".ext")))

    if(ext_file_exist_by_runno) {
      ext_file <- paste0(prefix,runno,".ext")
    } else if(ext_file_exist_by_file){
      ext_file <- paste0(gsub(ext_tab, "", file_tab),".ext")
    } else {
      ext_file <- list.files(path = directory, pattern = "\\.ext$")
    }
    
    # Check if there are multiple/ or no .ext files found in the directory (only problem if alternative reader is used --> reading without .lst/.ctl file or runno)
    
    if(length(ext_file) != 1) {
    if(length(ext_file) == 0) {
      warning("There is no .ext file in the directory\n")
    } else {
       warning(paste("There are multiple .ext files in the directory\n The current file is now being used:", ext_file[1]))
      ext_file <- ext_file[1]
    }
    }
    
    # Read .ext file
    parameters <- read_extfile(file = ext_file, project = directory, run = "", quiet = quiet)
    
    # Reformat omegas so that it fits pmx object
    
    momega <- parameters$omega
    domega <- as.double(diag(momega))
    names(domega) <- input_names[grepl("ETA", input_names)]
    omega <- data.table(
      EFFECT = sub("[.]?(eta|bsv)[.]?", "", names(domega)),
      OMEGA = sqrt(as.vector(domega)) ## omegas here is defined as standard deviation
    )
  
  ## Formatting the output for the parameters
    
    df_val <- as.data.frame(t(parameters$df)) #extract values form param list ITERATION -1E9
    df_se <- as.data.frame(t(parameters$df2)) #extract SE form param list ITERATION -100000001
  
    if(ncol(df_se) != 0) { #check if  ITERATION -1000000001 is exported
      df_param <- cbind(rownames(df_val),df_val, df_se,(df_se/df_val)*100)
      rownames(df_param) <- NULL
      colnames(df_param) <- c("PARAM","VALUE","SE","RSE")
      df_param <- df_param[-which(df_param$PARAM == "ITERATION"),]
      df_param <- dplyr::filter(df_param,PARAM != "ITERATION")
    } else {
      df_param <- cbind(rownames(df_val),df_val,NA,NA)
      rownames(df_param) <- NULL
      colnames(df_param) <- c("PARAM","VALUE","SE","RSE")
      df_param <- df_param[-which(df_param$PARAM == "ITERATION"),]
    }
  
  
  ## Generate the controller
    
    # generating the simulation object for the simulation dataset
    if(!is.null(dt_sim)) {
      sim <- pmx_sim(data = dt_sim, idv = "TIME", irun = "REP")
    } else {
      sim = NULL
    }
    
    
    # assign settings
    
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
      parameters = df_param
    )
    class(config) <- "pmxConfig"

  pmxClass$new(directory, input, dv, config, dvid, cats, conts, occ, strats, settings, endpoint, sim, bloq, time, sim_blq)
  
}



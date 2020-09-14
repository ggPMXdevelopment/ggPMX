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
                      file = NULL,
                      ext = ".lst",
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
                      obs = FALSE, 
                      time = "TIME",
                      npde,
                      iwres,
                      ipred, 
                      xp_reader = FALSE,
                      manual_import = NULL,
                      ignore = NULL,
                      simtab = NULL,
                      simfile = NULL,
                      prefix = "run") {
  
  ##
  config <- "standing"
  occ <- ""
  finegrid <- NULL
  
  
  ## Read the table files.
    
    dir <- directory
    table_vec <- c("sdtab","mutab","patab","catab","cotab","mytab","extra","xptab","cwtab")
    
    tab_man_specified <- NULL
    if(!identical(table_vec,table_names)) {
      tab_man_specified <- TRUE
    }
    
    
    if (is.null(runno) && is.null(file) && is.null(tab_man_specified)) {
      stop('Argument `runno`, `file` or  `table_names` required.', call. = FALSE)
    }
      
    
    # Check extensions
    if (!is.null(runno)) {
      ext <- make_extension(ext)
      full_path <- file_path(dir, stringr::str_c(prefix, runno, ext))
    } else {
      ext <- get_extension(file)
      if(length(ext) == 0) {
        ext <- ".lst"
        file <- "nofile"
      }
      if (ext == '') stop('An extension should be provided in the `file` name.', call. = FALSE)
      full_path <- file_path(dir, file)
    }
    
    alternative_import = FALSE
      if(!file.exists(full_path)){
        cat("Alternative import is used without model file\n")
        warning("No model file was found, check naming of files\n")
        alternative_import = TRUE
        file = NULL
      }
    
    
    ## Alternative report with loading model files (e.g. similar to xpose4)
    if(alternative_import){
      software   <- 'nonmem'
      tab_list <- manual_nm_import(tab_names = table_names, tab_suffix = table_suffix, sim_suffix = sim.suffix)
      
      
      if(is.null(runno)){
        
        cat()
        tbl_names <- file.path(dir,tab_list$tab_names)
        
        data <- tryCatch(read_nm_tables(file = tbl_names, dir = NULL, 
                                        quiet = quiet, simtab = FALSE, user_mode = FALSE), 
                         error = function(e) {
                           warning(e$message, call. = FALSE)
                           return()
                         })
        
      } else {
        
        tbl_names <- list_nm_tables_manual(runno = runno, dir = dir, tab_list = tab_list)
        
        data <- tryCatch(read_nm_tables(file = tbl_names, dir = NULL, 
                                        quiet = quiet, simtab = FALSE), ##cannot handle if cotab/patab are not long enough as sdtab
                         error = function(e) {
                           warning(e$message, call. = FALSE)
                           return()
                         })
        
      }

    }
    
    if(!alternative_import) {
      # List tables
      if (ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
        software   <- 'nonmem'
        model_code <- read_nm_model(file = basename(full_path), 
                                    dir  = dirname(full_path))
        
        if (is.null(manual_import)) {
          tbl_names <- list_nm_tables(model_code)
        } else {
          tbl_names <- list_nm_tables_manual(runno = runno, file = basename(full_path), 
                                             dir = dirname(full_path), tab_list = manual_import)
        }
      } else {
        stop('Model file currently not supported by read_nm_tables', call. = FALSE)
      }  
      
      
      # Import estimation tables
      if ('data' %in% ignore) {
        msg('Ignoring data import.', quiet)
        data <- NULL
      } else if (software == 'nonmem') {
        data <- tryCatch(read_nm_tables(file = tbl_names, dir = NULL, 
                                        quiet = quiet, simtab = simtab), 
                         error = function(e) {
                           warning(e$message, call. = FALSE)
                           return()
                         })
      } 
      
    }
    
    
    tmp <- data$data[data$simtab == FALSE]
    tmp <- as.data.table(tmp)
    
    sim_tmp <- data$data[data$simtab == TRUE]
    
    if(length(tmp) == 0){
      stop("Error, no data could be loaded")
    }
    
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
  
  ## Rename variables which cannot be specified in the pmx_nm() function, which rely on convetions
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
      cat(paste(y,"has been specified as",x,"\n"))
      return(data)
    } else if (length(names(data)[grep(z,names(data))]) == 0) {
      y <- names(data)[grep(z,names(data))]
      warning(paste(z,"not found in dataset! Please check naming of",x))
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
  
  
  
  #dt_sim <- as.data.table(simtmp[[1]]) ##read simulaiton file

  
if(!is.null(simfile)){
  vpc <- TRUE
}
  
  
dt_sim <- NULL    
if(vpc & !alternative_import & length(sim_tmp) == 0) {
  
  file <- simfile
  
  # Check extensions
  if (!is.null(runno)) {
    ext <- make_extension(ext)
    full_path <- file_path(dir, stringr::str_c(prefix, runno, ext))
  } else {
    ext <- get_extension(file)
    if (ext == '') stop('An extension should be provided in the `file` name.', call. = FALSE)
    full_path <- file_path(dir, file)
  }
  
  # List tables
  if (ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
    software   <- 'nonmem'
    model_code <- read_nm_model(file = basename(full_path), 
                                dir  = dirname(full_path))
    
    if (is.null(manual_import)) {
      tbl_names <- list_nm_tables(model_code)
    } else {
      tbl_names <- list_nm_tables_manual(runno = runno, file = basename(full_path), 
                                         dir = dirname(full_path), tab_list = manual_import)
    }
  } else {
    stop('Model file currently not supported by xpose.', call. = FALSE)
  }  
  
  # Import estimation tables
  simdata <- tryCatch(read_nm_tables(file = tbl_names, dir = NULL, 
                                     quiet = quiet, simtab = simtab), 
                      error = function(e) {
                        warning(e$message, call. = FALSE)
                        return()
                      })
  
  
  dt_sim <- as.data.table(simdata$data)
  
  sim_name_vec <- c("REP","ID","TIME","DV")
  dt_sim <- dt_sim[,..sim_name_vec]
  
  #dt_sim <- dt_sim[dt_sim$EVID == 0,]
  
  
}
  
  if(length(sim_tmp) != 0 & vpc) {
    dt_sim <- as.data.table(sim_tmp)
    
    sim_name_vec <- c("REP","ID","TIME","DV")
    dt_sim <- dt_sim[,..sim_name_vec]
    
  }

  
  if(vpc & alternative_import) {
    
    simdata <- tryCatch(read_nm_tables(file = tbl_names, dir = NULL, 
                                    quiet = quiet, simtab = TRUE), 
                     error = function(e) {
                       warning(e$message, call. = FALSE)
                       return()
                     })
    
    
    dt_sim <- as.data.table(simdata$data)
    
    #dt_sim <- dt_sim[dt_sim$EVID == 0,]
    
    sim_name_vec <- c("REP","ID","TIME","DV")
    dt_sim <- dt_sim[,..sim_name_vec]
    
  }
  
  ## remove missing rows if obs = TRUE (if obs = FALSE, missing values (MDV == 1) will be kept)
  
  MDV <- c()
  
  if(obs){
    if(any("MDV"==names(tmp))){
      tmp <- dplyr::filter(tmp,MDV==0)   
    } else {
      warning('\nMDV item not listed in header, 
              could not remove missing values!\n')
    }
  }
  
  ## remove non-observation rows
  
  if(any("EVID"==names(tmp))) {
        tmp <- dplyr::filter(tmp,EVID==0) 
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
  
  
  ##Generater the controller
  if(!is.null(dt_sim)) {
    sim <- pmx_sim(data = dt_sim, idv = "TIME", irun = "REP")
  } else {
    sim = NULL
  }

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
  

  
  pmxClass$new(directory, input, dv, config, dvid, cats, conts, occ, strats, settings, endpoint, sim, bloq, time) ##add TIME!
  
}



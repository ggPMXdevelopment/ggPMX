################
is.readable.file <- function(filename)
{
  
  ## If we are not dealing with R -> Splus
  if(is.null(version$language)) {
    cat("This version needs to be run with R")
    ## if(platform() == "WIN386") {
    ##   access(filename, 4) == 0
    ## } else {
    ##   filename <- paste("'", filename, "'", sep = "")
    ##   sapply(paste("test -f", filename, "-a -r", filename), unix,
    ##          output = F) == 0
    ## }
  } else {
    return(file.exists(filename)[1])
  }
  
}

####################3
read.nm.tables <- function (table.files = NULL, runno = NULL, tab.suffix = "", 
          table.names = c("sdtab", "mutab", "patab", "catab", "cotab", 
                          "mytab", "extra", "xptab"), cwres.name = c("cwtab"), 
          cwres.suffix = "", quiet = FALSE, new_methods = TRUE, ...) 
{
  if (is.null(table.files)) {
    if (is.null(runno)) {
      cat(paste("runno must be specified if no table files provided\n"))
      return(NULL)
    }
    match.pos <- match(cwres.name, table.names)
    if (!is.na(match.pos)) 
      table.names <- table.names[-match.pos]
    tab.files <- sapply(table.names, paste, runno, tab.suffix, 
                        sep = "")
    cwres.files <- sapply(cwres.name, paste, runno, cwres.suffix, 
                          tab.suffix, sep = "")
    tab.files <- c(tab.files, cwres.files)
  }
  else {
    tab.files <- table.files
  }
  totab <- NULL
  totnam <- NULL
  seen.files <- NULL
  filedim <- NULL
  for (i in 1:length(tab.files)) {
    filename <- tab.files[i]
    if (!is.readable.file(filename)) {
      next
    }
    else {
      cat(paste("    Reading", filename, "\n"))
      if (new_methods) {
        assign(paste0("n.", filename), read_nm_table(filename, 
                                                     quiet = quiet, ...))
      }
      else {
        header.line = scan(file = filename, nlines = 1, 
                           skip = 1, what = "character", sep = "\n", 
                           quiet = T)
        sep.char = ""
        if (length(grep(",", header.line)) != 0) 
          sep.char = ","
        fields.per.line <- count.fields(filename)
        fields.in.first.line <- fields.per.line[1]
        fields.in.rest <- fields.per.line[-1]
        if ((length(unique(fields.in.rest)) != 1) || 
            (all(fields.in.first.line == fields.in.rest))) {
          if (!quiet) {
            cat(paste("Found different number of fields in ", 
                      filename, ".\n", sep = ""))
            cat("This may be due to multiple TABLE and header rows \n")
            cat("caused by running multiple simulations in NONMEM (NSIM > 1).\n")
            cat("Will try to remove these rows. It may take a while...\n")
          }
          tmp <- readLines(filename, n = -1)
          inds <- grep("TABLE", tmp)
          if (length(inds) != 1) {
            inds <- inds[c(2:length(inds))]
            inds2 <- inds + 1
            tempfile <- paste(filename, ".xptmp", sep = "")
            write.table(tmp[-c(inds, inds2)], file = tempfile, 
                        row.names = FALSE, quote = FALSE)
            assign(paste("n.", filename, sep = ""), 
                   read.table(tempfile, skip = 2, header = T, 
                              sep = sep.char))
            unlink(tempfile)
          }
          else {
            assign(paste("n.", filename, sep = ""), 
                   read.table(filename, skip = 1, header = T, 
                              sep = sep.char))
          }
        }
        else {
          assign(paste("n.", filename, sep = ""), read.table(filename, 
                                                             skip = 1, header = T, sep = sep.char))
        }
      }
      seen.files <- c(seen.files, paste("n.", filename, 
                                        sep = ""))
    }
  }
  if (any(is.null(seen.files))) {
    cat("Couldn't find any table files that match run number", 
        runno, "!\n")
    return(NULL)
  }
  for (nfile in seen.files) {
    if (is.null(filedim)) {
      filedim <- nrow(get(nfile))
    }
    else {
      filedim <- c(filedim, nrow(get(nfile)))
    }
  }
  file.df <- data.frame(seen.files = seen.files, filedim = filedim)
  lngths <- sort(unique(file.df$filedim))
  if (length(lngths) != 1) {
    cat("\nThe table files associated with this run number (", 
        runno, ") appear\n")
    cat("to have different lengths.\n")
    cat("You will have to sort this out and try again!\n")
    return(NULL)
  }
  maxlngth <- max(file.df$filedim)
  for (ii in 1:nrow(file.df)) {
    filnam <- as.character(file.df[ii, "seen.files"])
    new.df <- get(filnam)
    sz <- file.df[ii, "filedim"]
    rl <- maxlngth/sz
    if (any(is.null(totab))) {
      totab <- cbind(new.df)
    }
    else {
      totab <- cbind(totab, new.df)
    }
    totnam <- c(totnam, names(new.df))
    if (!is.na(pmatch("n.patab", filnam))) {
      write(names(new.df), file = ".patab.names.tmp")
    }
    else {
      if (!is.na(pmatch("n.catab", filnam))) {
        write(names(new.df), file = ".catab.names.tmp")
      }
      else {
        if (!is.na(pmatch("n.cotab", filnam))) {
          write(names(new.df), file = ".cotab.names.tmp")
        }
        else {
          if (!is.na(pmatch("n.sdtab", filnam))) {
            write(names(new.df), file = ".sdtab.names.tmp")
          }
        }
      }
    }
  }
  totab <- totab[, !duplicated(totnam)]
  return(totab)
}

#################
read_nm_table <- function (nm_table, only_obs=FALSE, method="default",quiet=TRUE,sim_num=FALSE,sim_name="NSIM") {
  
  if(method=="default") method <- "readr_1"
  
  read_nm_tab_readr_1 <- function(nm_table,sim_num){
    
    #tab_dat <- read_table(nm_table, skip = 1) 
    #tab_dat <- tab_dat %>% mutate_each(funs(suppressWarnings(as.numeric(.))))
    
    ## get header names
    header_line <- readr::read_lines(nm_table,n_max=2)[2]
    comma_sep <- FALSE
    if(length(grep(",",header_line))!=0) comma_sep <- TRUE
    header_line <- sub("^\\s+","",header_line) 
    header_names <- strsplit(header_line,"\\s+,*\\s*")[[1]]
    
    #final_line <- readr::read_lines(nm_table,n_max=2)[-1]
    
    if(!comma_sep){
      # check if it is fixed width or not
      data_lines <- readr::read_lines(nm_table,n_max=10)[-c(1,2)]
      fixed_width <- FALSE
      if(length(unique(nchar(data_lines)))==1) fixed_width <- TRUE
      if(fixed_width){##change to fixedlength
        tab_dat <- readr::read_table(nm_table, col_names = header_names, 
                                     col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                     skip = 2,na=c("NA")) 
      } else {
        tab_dat <- readr::read_delim(nm_table, delim=" ",skip=2,
                                     col_names = header_names,
                                     col_types=paste0(rep("d",length(header_names)),collapse = ""))
        
      }
    } else {
      tab_dat <- readr::read_csv(nm_table, col_names = header_names, 
                                 col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                 skip = 2) 
    }
    
    # Handle multiple simulations
    if(any(is.na(tab_dat[1]))){ 
      if(sim_num){
        ## create simulation number
        args <- lazyeval::interp(~ cumsum(is.na(var))+1, var = as.name(names(tab_dat)[1]))
        tab_dat <- dplyr::mutate_(tab_dat,NSIM=args)
      }
      
      ## filter out NA columns
      args <- lazyeval::interp(~ !is.na(var), var = as.name(names(tab_dat)[1]))
      tab_dat <- dplyr::filter_(tab_dat,args)
    }
    return(tab_dat)
  }
  
  
  read_nm_tab_readr_2 <- function(nm_table){
    ## get header names
    header_line <- readr::read_lines(nm_table,n_max=2)[2]
    comma_sep <- FALSE
    if(length(grep(",",header_line))!=0) comma_sep <- TRUE
    header_line <- sub("^\\s+","",header_line) 
    header_names <- strsplit(header_line,"\\s+,*\\s*")[[1]]
    
    
    ## Check if we have unequal number of fields in the file
    ## used for multiple simulations
    tmp   <- readr::read_lines(nm_table)
    inds  <- grep("TABLE",tmp)
    if (length(inds)!=1){
      inds  <- inds[c(2:length(inds))]
      inds2 <- inds+1
      tempfile<- paste(nm_table,".xptmp",sep="")
      write.table(tmp[-c(inds,inds2)],file=tempfile,
                  row.names=FALSE,quote=FALSE,col.names = FALSE)
      #assign(paste("n.",filename,sep=""),read.table(tempfile,skip=2,header=T,sep=sep.char))
      if(!comma_sep){
        tab_dat <- readr::read_table(tempfile, col_names = header_names, 
                                     col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                     skip = 2) 
      } else {
        tab_dat <- readr::read_csv(tempfile, col_names = header_names, 
                                   col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                   skip = 2) 
      }
      unlink(tempfile)
    } else {
      if(!comma_sep){
        tab_dat <- readr::read_table(nm_table, col_names = header_names, 
                                     col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                     skip = 2) 
      } else {
        tab_dat <- readr::read_csv(nm_table, col_names = header_names, 
                                   col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                   skip = 2) 
      }
    }
    
    return(tab_dat)
  }
  
  read_nm_tab_readr_3 <- function(nm_table,sim_num){
    ## get header names
    header_line <- readr::read_lines(nm_table,n_max=2)[2]
    comma_sep <- FALSE
    if(length(grep(",",header_line))!=0) comma_sep <- TRUE
    header_line <- sub("^\\s+","",header_line) 
    header_names <- strsplit(header_line,"\\s+,*\\s*")[[1]]
    
    # read in all lines of file
    tmp   <- readr::read_lines(nm_table)
    #tmp_table <- nm_table
    tmp_table <- paste(tmp,collapse="\n")
    fun_name <- "readr::read_table"
    if(comma_sep) fun_name <- "readr::read_csv"
    skip=2
    
    ## Check for multiple table lines
    inds  <- grep("TABLE",tmp)
    if (length(inds)!=1){
      inds2 <- inds+1 # additional header lines
      if(sim_num){
        NSIM <- rep(1,length(tmp))
        NSIM[inds] <- NA
        NSIM <- cumsum(is.na(NSIM))
        tmp <- paste(tmp,NSIM) 
        header_names <- c(header_names,"NSIM")
      }
      tmp_table <- paste(tmp[-c(inds,inds2)],collapse="\n")
      skip=0
    }
    tab_dat <- do.call(eval(parse(text=paste0(fun_name))),args = list(tmp_table,col_names = header_names, 
                                                                      col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                                                      skip = skip))
    
    return(tab_dat)
  }
  
  
  
  
  read_nm_tab_slow <- function (filename, quiet) {
    ## Check which type of separator we have in our tables
    header.line = scan(file=filename,nlines=1,skip=1,what="character",sep="\n",quiet=T)
    sep.char = ""
    if(length(grep(",",header.line))!=0) sep.char = ","
    
    ## Check if we have unequal number of fields in the file
    ## used for multiple simulations
    fields.per.line <- count.fields(filename)
    fields.in.first.line <- fields.per.line[1]
    fields.in.rest <- fields.per.line[-1]
    if((length(unique(fields.in.rest))!=1) ||
       (all(fields.in.first.line==fields.in.rest))){ 
      if(!quiet) {
        cat(paste("Found different number of fields in ",filename,".\n",sep=""))
        cat("This may be due to multiple TABLE and header rows \n")
        cat("caused by running multiple simulations in NONMEM (NSIM > 1).\n")
        cat("Will try to remove these rows. It may take a while...\n")
      }
      tmp   <- readLines(filename, n = -1)
      inds  <- grep("TABLE",tmp)
      if (length(inds)!=1){
        inds  <- inds[c(2:length(inds))]
        inds2 <- inds+1
        tempfile<- paste(filename,".xptmp",sep="")
        write.table(tmp[-c(inds,inds2)],file=tempfile,
                    row.names=FALSE,quote=FALSE)
        #assign(paste("n.",filename,sep=""),read.table(tempfile,skip=2,header=T,sep=sep.char))
        tab_dat <- read.table(tempfile,skip=2,header=T,sep=sep.char)
        unlink(tempfile)
      } else {
        #assign(paste("n.",filename,sep=""),read.table(filename,skip=1,header=T,sep=sep.char))
        tab_dat <- read.table(filename,skip=1,header=T,sep=sep.char)
      }
    } else {
      #assign(paste("n.",filename,sep=""),read.table(filename,skip=1,header=T,sep=sep.char))
      tab_dat <- read.table(filename,skip=1,header=T,sep=sep.char)
    }
    return(tab_dat)
  }
  
  tab_dat <- switch(method,
                    readr_1 = read_nm_tab_readr_1(nm_table, sim_num=sim_num),
                    readr_2 = read_nm_tab_readr_2(nm_table),
                    readr_3 = read_nm_tab_readr_3(nm_table,sim_num=sim_num),
                    slow = read_nm_tab_slow(nm_table,quiet=quiet)
  )
  
  ## remove non-observation rows
  MDV <- c()
  if(only_obs){
    if(any("MDV"==names(tab_dat))){
      tab_dat <- dplyr::filter(tab_dat,MDV==0)   
    } else {
      warning('\nMDV data item not listed in header, 
              Could not remove dose events!\n')
    }
  }
  
  if(sim_num) names(tab_dat)[match("NSIM",names(tab_dat))] <- sim_name
  
  tab_dat <- data.frame(tab_dat)
  tab_dat <- dplyr::as_data_frame(tab_dat)
  
  return(tab_dat)
}
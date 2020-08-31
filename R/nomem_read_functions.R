################
#' Title
#'
#' @param filename 
#'
#' @return
#' @noRd
#'
#' @examples
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
#' Title
#'
#' @param table.files 
#' @param runno 
#' @param table_suffix 
#' @param table_names 
#' @param cwres.name 
#' @param cwres.suffix 
#' @param quiet 
#' @param new_methods 
#' @param ... 
#'
#' @return
#' @noRd
#'
#' @examples
read.nm.tables <- function (table.files = NULL, runno = NULL, table_suffix = "", 
          table_names = c("sdtab", "mutab", "patab", "catab", "cotab", 
                          "mytab", "extra", "xptab"), cwres.name = c("cwtab"), 
          cwres.suffix = "", quiet = FALSE, new_methods = TRUE, ...) 
{
  if (is.null(table.files)) {
    if (is.null(runno)) {
      cat(paste("runno must be specified if no table files provided\n"))
      return(NULL)
    }
    match.pos <- match(cwres.name, table_names)
    if (!is.na(match.pos)) 
      table_names <- table_names[-match.pos]
    tab.files <- sapply(table_names, paste, runno, table_suffix, 
                        sep = "")
    cwres.files <- sapply(cwres.name, paste, runno, cwres.suffix, 
                          table_suffix, sep = "")
    tab.files <- c(tab.files, cwres.files)
  }
  else {
    tab.files <- table.files
  }
  totab <- NULL
  totnam <- NULL
  seen.files <- NULL
  filedim <- NULL
  table.list <- list()
  ca_tmp <- NULL
  co_tmp <- NULL
  sd_tmp <- NULL
  
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
      
      if(grepl("catab", filename)) { 
        ca_tmp <- read_header(filename)
      }
      
      if(grepl("cotab", filename)) { 
        co_tmp <- read_header(filename)
      }
      
      if(grepl("sdtab", filename)) { 
        sd_tmp <- read_header(filename)
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
  
  table.list <- list(totab,
                     ca_tmp,
                     co_tmp,
                     sd_tmp,
                     "")
  return(table.list)
  

}

#################
#' Title
#'
#' @param nm_table 
#' @param only_obs 
#' @param method 
#' @param quiet 
#' @param sim_num 
#' @param sim_name 
#'
#' @return
#' @noRd
#'
#' @examples
read_nm_table <- function (nm_table, only_obs=FALSE, method="default",quiet=TRUE,sim_num=FALSE,sim_name="NSIM") {
  
    ## get header names
    header_line <- readr::read_lines(nm_table,n_max=2)[2]
    comma_sep <- FALSE
    if(length(grep(",",header_line))!=0) comma_sep <- TRUE
    header_line <- sub("^\\s+","",header_line) 
    header_names <- strsplit(header_line,"\\s+,*\\s*")[[1]]
    
    #final_line <- readr::read_lines(nm_table,n_max=2)[-1]
    
    if(!comma_sep){
        tab_dat <- readr::read_table(nm_table, col_names = header_names, 
                                     col_types=paste0(rep("d",length(header_names)),collapse = ""),
                                     skip = 2,na=c("NA")) 
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
  
  if(sim_num) names(tab_dat)[match("NSIM",names(tab_dat))] <- sim_name
  
  tab_dat <- data.frame(tab_dat)
  tab_dat <- dplyr::as_data_frame(tab_dat)
  
  return(tab_dat)
}


##########################

#' Title
#'
#' @param run 
#' @param project 
#' @param file 
#' @param path 
#' @param read_fun 
#'
#' @return
#' @noRd
#'
#' @examples
read_nmext <- function(run=NA_real_, project = getwd(), file=paste0(run,".ext"), 
                       path=NULL, read_fun = c("data.table","read.table")) {
  
  if(is.character(path)) {
    extfile <- path  
  } else {
    extfile <- file.path(project,run,file)
  }
  
  if(!file.exists(extfile)) {
    stop("[read_nmext] could not find the requested 'ext' file ", 
         shQuote(basename(extfile)))
  }
  
  read_fun <- match.arg(read_fun)
  
  use_dt <- requireNamespace("data.table",quietly=TRUE) & read_fun=="data.table"
  
  ##Check for multiple tables
  ext_tmp <- readr::read_lines(extfile,n_max=-1)
  inds  <- grep("TABLE",ext_tmp)
  last_table <- 0
  
  if (length(inds)!=1){
    last_table <- inds[length(inds)]
    colon <- regexpr(":",ext_tmp[last_table])[1]
    last_table_name <- substr(ext_tmp[last_table],1,colon-1)
    last_table <- last_table-1
    warning(paste("Multiple Problems found in",file,"only using",last_table_name,"used \n")) #multiple problems not currently supported add to vignette
  }
  
  
  
  if(use_dt) {
    df <- data.table::fread(
      file=extfile, 
      na.strings = '.', 
      data.table=FALSE,
      skip=last_table+1
    )
  } else {
    df <- read.table(
      file=extfile,
      na.strings='.',
      stringsAsFactors=FALSE,
      skip=last_table+1, 
      header=TRUE
    )
  }
  
  ans <- df[df[["ITERATION"]] == -1E9,]
  
  if(nrow(ans)==0) {
    stop(
      "[read_nmext] could not find final estimates",
      " while reading 'ext' file ", shQuote(basename(extfile))
    )
  }
  
  ans <- as.list(ans)
  names(ans) <- gsub("[[:punct:]]", "", names(ans))
  ans <- list(
    param = ans[grepl("THETA", names(ans))],
    omega = mrgsolve::as_bmat(ans, "OMEGA"), 
    sigma = mrgsolve::as_bmat(ans, "SIGMA"),
    raw = ans  
  )
  return(ans)
}



########################

read_header <- function(nm_table) {
  header_line <- readr::read_lines(nm_table,n_max=2)[2]
  comma_sep <- FALSE
  if(length(grep(",",header_line))!=0) comma_sep <- TRUE
  header_line <- sub("^\\s+","",header_line) 
  header_names <- strsplit(header_line,"\\s+,*\\s*")[[1]]
  
  return(header_names)
  
}





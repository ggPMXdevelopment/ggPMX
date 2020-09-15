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
    warning(paste("Multiple Problems found in",file,"only using",last_table_name,"\n")) #multiple problems not currently supported
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
  
  ans <- ""
  ans <- df[df[["ITERATION"]] == -1E9,]
  
  ans_se <- ""
  ans_se <- df[df[["ITERATION"]] == -1000000001,]
  
  df_ans <- ans
  
  df_ans_se <- ans_se
  
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
    raw = ans,
    df = df_ans,
    df2 = df_ans_se
  )
  return(ans)
}


########################




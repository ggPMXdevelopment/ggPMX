#' Creates parameter kable
#'
#' @param ctr Generated controller from e.g. \code{\link{pmx_mlx}} for Monolix.
#' @param return_table If TRUE, returns the same table as in get_data('estimates') otherwise it returns a kable
#' @param fun \code{character} can be "sd" or "var" for shrinkage computation, see \code{\link{pmx_comp_shrink}}
#'
#' @return Returns a kable with the parameter estimates from get_data('estimates')
#' @export
#'
#' @examples
#' #ctr <- theophylline()
#' #my_params <- ctr %>% param_table(fun = "var")

param_table <- function(ctr,fun,return_table = FALSE) {

  ## Avoid error message in cmd check
  
  SE <- EFFECT <- SHRINK <- RSE <- VALUE <- PARAM <- . <- NULL
  
  
  pop_pars = ctr %>% get_data('estimates')
  rownames(pop_pars) <- trimws(as.character(pop_pars[["PARAM"]]))
  
  if(ctr$config$sys == "mlx" | ctr$config$sys == "mlx18" ){
    
    if(missing(fun)) fun <- c("var","sd")
    
    if(identical(fun,c("var","sd"))){
      fun = "var"
      msg <- "`var` was used for shrinkage calculation"
      message(msg)
    } else if (fun == "var") {
      msg <- "`var` was used for shrinkage calculation"
      message(msg)
    } else if (fun == "sd") {
      msg <- "`sd` was used for shrinkage calculation"
      message(msg)
    }
    
    shrinkage = ctr %>% pmx_comp_shrink(fun=fun) %>% 
      dplyr::mutate(SHRINK=paste0(round(SHRINK*100),'%')) %>% # make % shrinkage
      dplyr::mutate(PARAM=paste0('omega_',EFFECT)) %>%  #generate new column PARAM with "omega_" prefix
      dplyr::select(PARAM,SHRINK) %>% #select PARAM and SHRINK (%)
      dplyr::mutate(PARAM = as.character(PARAM))
    
    pop_pars2 = pop_pars %>% 
      dplyr::mutate(VALUE=as.character(signif(VALUE,2)),RSE=ifelse(is.na(RSE), 'fixed',paste0(round(RSE),'%'))) %>% #make RSE in %, and NaN to "fixed"
      dplyr::select(PARAM,VALUE,RSE) %>% #select param and value
      dplyr::mutate(PARAM = as.character(PARAM)) %>% #set param as character
      dplyr::arrange(PARAM) %>% #sort according to param
      dplyr::mutate(PARAM = trimws(PARAM)) %>% #remove white space
      dplyr::left_join(shrinkage, by = "PARAM") %>% #join shrinkage
      dplyr::mutate(SHRINK=ifelse(is.na(SHRINK),'',SHRINK)) # remove NAs from empty shrink
    
    pop_pars3 = suppressWarnings(  dplyr::bind_rows(data.frame(PARAM='.',VALUE='', RSE ='', SHRINK='')) %>%
                                   dplyr::bind_rows(pop_pars2 %>% dplyr::filter(grepl('_pop',PARAM))) %>% 
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='', RSE ='', SHRINK='')) %>%
                                   dplyr::bind_rows(pop_pars2 %>% dplyr::filter(grepl('omega_',PARAM))) %>% 
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='', RSE ='', SHRINK='')) %>%
                                   dplyr::bind_rows(pop_pars2 %>% dplyr::filter(grepl('beta_',PARAM))) %>% 
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='',RSE ='', SHRINK='')) %>%
                                   dplyr::bind_rows(pop_pars2 %>% dplyr::filter(PARAM %in% c('a','b'))) %>% 
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='',RSE ='', SHRINK=paste("comp. with",fun))) %>% 
                                   dplyr::rename(Parameter=PARAM, Estimate=VALUE, Relative_SE=RSE,Shrinkage=SHRINK))
  }
  
  if(ctr$config$sys == "nm"){ 
    
    #currently the shrinkage is calulcated using sd for nonmem, pmx_comp_shrink uses values from get_data("omega") which is the "sd" for nonmem
    if(missing(fun)) fun <- c("var","sd")
    
    if(identical(fun,c("var","sd"))){
      fun = "sd"
      msg <- "`sd` was used for shrinkage calculation"
      message(msg)
    } else if (fun == "var") {
      msg <- "`var` was used for shrinkage calculation"
      message(msg)
    } else if (fun == "sd") {
      msg <- "`sd` was used for shrinkage calculation"
      message(msg)
    }
    
    shrinkage = ctr %>% pmx_comp_shrink(fun = fun) %>% #because for nonmem data is used of get_data("omega"), which is the SD
      dplyr::mutate(SHRINK=paste0(round(SHRINK*100),'%')) %>% # make % shrinkage
      dplyr::mutate(PARAM=EFFECT) %>%  #generate new column PARAM with "omega_" prefix
      dplyr::select(PARAM,SHRINK) %>% #select PARAM and SHRINK (%)
      dplyr::mutate(PARAM = as.character(PARAM))
    
    i = 1
    for(i in i:length(shrinkage$PARAM)) {
      str <- shrinkage$PARAM[i]
      num <- as.numeric(substring(str,4,))
      shrinkage$PARAM[i] <- paste0("OMEGA(",num,",",num,")")
    }

    pop_pars2 = pop_pars %>% 
      dplyr::mutate(RSE=round(RSE)) %>% 
      dplyr::mutate(RSE=ifelse(SE == 1E10, "fixed", RSE)) %>% 
      dplyr::mutate(VALUE=as.character(signif(VALUE,2)),RSE=ifelse(RSE != "fixed",paste0(RSE,'%'), RSE)) %>%
      dplyr::select(PARAM,VALUE,RSE) %>% #select param and value
      dplyr::mutate(PARAM = as.character(PARAM)) %>% #set param as character
      dplyr::arrange(PARAM) %>% #sort according to param
      dplyr::mutate(PARAM = trimws(PARAM)) %>% #remove white space
      dplyr::left_join(shrinkage, by = "PARAM") %>% #join shrinkage
      dplyr::mutate(SHRINK=ifelse(is.na(SHRINK),'',SHRINK)) # remove NAs from empty shrink

    pop_pars3 =  suppressWarnings(dplyr::bind_rows(pop_pars2 %>% dplyr::filter(grepl('OBJ',PARAM))) %>% 
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='', RSE ='', SHRINK='')) %>%
                                   dplyr::bind_rows(pop_pars2 %>% dplyr::filter(grepl('THETA',PARAM))) %>% 
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='', RSE ='', SHRINK='')) %>%
                                   dplyr::bind_rows(pop_pars2 %>% dplyr::filter(grepl('SIGMA',PARAM))) %>% 
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='', RSE ='', SHRINK='')) %>%
                                   dplyr::bind_rows(pop_pars2 %>% dplyr::filter(grepl('OMEGA',PARAM))) %>%
                                   dplyr::bind_rows(data.frame(PARAM='.',VALUE='',RSE ='', SHRINK=paste("comp. with",fun))) %>% 
                                   dplyr::rename(Parameter=PARAM, Estimate=VALUE, Relative_SE=RSE,Shrinkage=SHRINK))
  }
  
  if(ctr$config$sys == "nlmixr"){
    message("`param_table()` is not yet implemented for nlmixr")
  }
  
  if(return_table) {
    pop_pars3 <- as.data.table(pop_pars)
    return_kable <- FALSE
  } else {
    return_kable <- TRUE
  }
  
  if(return_kable) {
    pop_pars3 <- pop_pars3 %>% knitr::kable(col.names = c("Parameter", "Value", "Relative Standard Error","Shrinkage"), 
                                            caption=paste0('Parameter estimates (',nrow(pop_pars3)+1,' lines)'),align='lrrr')
  }
    return(pop_pars3)
}



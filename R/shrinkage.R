
#' Compute shrinkage
#'
#' @param estimates parameters estimation data 
#' @param eta individual estimation data
#' @param fun can be either sd or var
#' @param by \code{character} vector to group by before shrinkage
#'
#' @return data.table computing the shrinkage by effect
#' @importFrom stringr str_trim
#' @export

shrinkage <-
  function(estimates, eta, fun=c("sd","var"),by=""){
    PARAM <-  EFFECT <- VAR <- FUN <- NULL
    VALUE_ETA <- VALUE_OMEGA <- VALUE <- NULL
    if(missing(fun))fun <- "sd"
    dx1 <- 
      estimates[grepl("omega", PARAM)][
        , c("VAR","EFFECT") := list("omega",gsub("(^ +)?omega_","",PARAM))]
    dx2 <- eta[VAR == "eta" & grepl("mean", FUN)]
    if(nrow(dx2)==0)dx2 <- eta[VAR == "eta" & grepl("mode", FUN)]
    dx <- merge(dx2, dx1, by = "EFFECT")
    setnames(dx, c("VALUE.x", "VALUE.y"), c("VALUE", "VALUE_OMEGA"))
    
    grp <- "EFFECT,VALUE_OMEGA"
    if(any(nzchar(by)))grp <- sprintf('%s,%s',grp,paste(by,collapse = ","))
    dx[, {
      coef <- if(fun=="sd")VALUE_OMEGA else VALUE_OMEGA^2
      shrink <- 1 - get(fun)(VALUE) / coef 
      
      list(SHRINK=shrink,POS=max(VALUE)/2)
    }, grp]
    
  }









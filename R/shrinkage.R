
#' Compute shrinkage
#'
#' @param estimates parameters estimation data 
#' @param eta individual estimation data
#' @param sys data system
#' @param fun can be either sd or var
#'
#' @return data.table computing the shrinkage by effect
#' @importFrom stringr str_trim
#' @export

shrinkage <-
  function(estimates, eta, sys="mlx", fun=c("sd","var")){
    PARAM <- NULL; EFFECT <- NULL; VAR <- NULL; FUN <- NULL
    VALUE_ETA <- NULL; VALUE_OMEGA <- NULL
    if(missing(fun))fun <- "sd"
    dx1 <- 
      estimates[grepl("omega", PARAM)][
        , c("VAR","EFFECT") := list("omega",gsub("(^ +)?omega_","",PARAM))]
    dx2 <- eta[VAR == "eta" & grepl("mean", FUN)]
    if(nrow(dx2)==0)dx2 <- eta[VAR == "eta" & grepl("mode", FUN)]
    dx <- merge(dx2, dx1, by = "EFFECT")
    setnames(dx, c("VALUE.x", "VALUE.y"), c("VALUE_ETA", "VALUE_OMEGA"))
    dx[, list(SHRINK = 1 - get(fun)(VALUE_ETA) / 
                       if(fun=="sd")VALUE_OMEGA else VALUE_OMEGA^2), "EFFECT,VALUE_OMEGA"]
    
  }




shrinkage_data <-
  function(ctr,sys="mlx", fun="sd") {
    estimates <- ctr %>% get_data("estimates")
  eta <- ctr %>% get_data("eta")
  dx.etas <- eta[VAR == "eta" & grepl("mean", FUN)]
  shrink <- shrinkage(estimates, eta, sys, fun)
  dx <- merge(dx.etas, shrink, by = "EFFECT")
}



#' Create shrinkage layer
#'
#' @param ctr controller
#'
#' @return ggplot2 layer
#' @import ggplot2
#' @export
shrinkage_layer <- 
  function(ctr,type="hist",color="red",size=5,pos_=0.75,fun="sd",x_=0) {
  dx = shrinkage_data(ctr=ctr,fun=fun)[, list(
                   x=x_, 
                   pos = max(VALUE) * pos_,
                   label = sprintf('%s%%', 
                                   round(SHRINK[1]*100))
  ),EFFECT]
  if(type=="box")
    geom_text(data=dx,
    aes(x=EFFECT,label = label, y = pos), color = color, size = size)
  else  geom_text(data=dx,aes(x=x,y=pos, label=label),color = color, size = size)

}









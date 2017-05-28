
#' Compute shrinkage
#'
#' @param par_est parameters estimation data 
#' @param ind_pred individual estimation data
#' @param sys data system
#' @param fun can be either sd or var
#'
#' @return data.table computing the shrinkage by effect
#' @importFrom stringr str_trim
#' @export

shrinkage <-
  function(par_est, ind_pred, sys="mlx", fun=c("sd","var")){
    PARAM <- NULL; EFFECT <- NULL; VAR <- NULL; FUN <- NULL
    VALUE_ETA <- NULL; VALUE_OMEGA <- NULL
    dx1 <- 
      par_est[grepl("omega", PARAM)][
        , c("VAR","EFFECT") := tstrsplit(PARAM, "_")]
    dx1 <- dx1[, c("EFFECT", "VAR") := list(str_trim(tolower(EFFECT)),  
                                            str_trim(VAR))]
    
    dx2 <- ind_pred[VAR == "eta" & grepl("mean", FUN)]
    
    dx <- merge(dx2, dx1, by = "EFFECT")
    setnames(dx, c("VALUE.x", "VALUE.y"), c("VALUE_ETA", "VALUE_OMEGA"))
    unique(
           dx[, list(SHRINK = 1 - get(fun)(VALUE_ETA) / 
                       ifelse(fun=="sd",VALUE_OMEGA,VALUE_OMEGA^2)), EFFECT]
         )
    
  }




shrinkage_data <-
  function(ctr,sys="mlx", fun="sd") {
  par_est <- ctr %>% get_data("par_est")
  ind_pred <- ctr %>% get_data("ind_pred")
  dx.etas <- ind_pred[VAR == "eta" & grepl("mean", FUN)]
  shrink <- shrinkage(par_est, ind_pred, sys, fun)
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









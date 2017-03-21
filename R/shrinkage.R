
#' Compute shrinkage
#'
#' @param par_est parameters estimation data 
#' @param ind_pred individual estimation data
#' @param sys data system
#' @param fun can mean the sd or var
#'
#' @return data.table computing the shrinkage by effect
#' @importFrom stringr str_trim
#' @export

shrinkage <-
  function(par_est, ind_pred, sys="mlx", fun="sd"){
    PARAM <- NULL; EFFECT <- NULL; VAR <- NULL; FUN <- NULL
    VALUE.ETA <- NULL; VALUE.OMEGA <- NULL
    dx1 <- 
      par_est[grepl("omega", PARAM)][
        , c("VAR","EFFECT") := tstrsplit(PARAM, "_")]
    dx1 <- dx1[, c("EFFECT", "VAR") := list(str_trim(tolower(EFFECT)),  
                                            str_trim(VAR))]
    
    dx2 <- ind_pred[VAR == "eta" & grepl("mean", FUN)]
    
    dx <- merge(dx2, dx1, by = "EFFECT")
    setnames(dx, c("VALUE.x", "VALUE.y"), c("VALUE_ETA", "VALUE_OMEGA"))
    unique(
      dx[, list(SHRINK = 1 - get(fun)(VALUE_ETA) / VALUE_OMEGA), EFFECT]
    )
    
  }





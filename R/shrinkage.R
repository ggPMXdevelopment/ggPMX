



#' Compute shrinkage
#'
#' @param par_est data set
#' @param ind_pred data set
#' @param sys data system
#' @param fun can mean the sd or var
#'
#' @return data.table computing the shrinkage by effect
#' @importFrom stringr str_trim
#' @export

shrinkage <-
  function(par_est, ind_pred, sys="mlx", fun="sd"){
    PARAM <- NULL; EFFECT <- NULL; VAR <- NULL; FUN <- NULL
    VALUE.x <- NULL; VALUE.y <- NULL
    dx1 <- par_est[grepl("omega", PARAM), 
                   c("VAR","EFFECT") := tstrsplit(PARAM, "_")]
    dx1 <- dx1[, c("EFFECT", "VAR") := list(str_trim(tolower(EFFECT)),  
                                            str_trim(VAR))]
    dx1 <- dx1[VAR == "omega"]
    dx2 <- ind_pred[VAR == "eta" & grepl("mean", FUN)]
    unique(
      merge(dx2, dx1, by = "EFFECT")[
        , list(SHRINK = 1 - get(fun)(VALUE.x) / VALUE.y), EFFECT]
    )
  }





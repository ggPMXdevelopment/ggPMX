pmx_plot_generic <- 
  function(ctr,pname,defaults_,...){
    
    stopifnot(is_pmxclass(ctr))
    if(!pname %in% (ctr %>% plot_names))return(NULL)
    cctr <- pmx_copy(ctr) 
    params <- 
      c(
        ctr=cctr,
        pname=pname,
        l_left_join(defaults_,list(...))
      )
    do.call("pmx_update",params)
    p <- cctr %>%  get_plot(pname)
    rm(cctr)
    p
  }

# DV vs PRED plot --------------------------------------------------------------

#' DV vs PRED plot
#' @param ctr pmx controller 
#' @param ... others graphics parameters passed to \code{\link{pmx_gpar}}.
#' @param ... others graphics parameters passed to \code{\link{residual}} internal object.
#' @param ... others graphics parameters passed to \code{\link{pmx_update}} internal object.
#' @return ggplot2 object
#' @export
pmx_plot_dv_pred <- function(ctr,...){
  pmx_plot_generic(ctr,"dv_pred",defaults_dv_pred,...)
}


# DV vs IPRED plot --------------------------------------------------------------

#' DV vs IPRED plot
#' @return ggplot2 object
#' @export
pmx_plot_dv_ipred <- function(
  ctr,
  ...){
  pmx_plot_generic(ctr,"dv_ipred",defaults_dv_ipred,...)
}


# IWRES vs IPRED plot --------------------------------------------------------------


#' IWRES vs IPRED plot
#' @return ggplot2 object
#' @export
pmx_plot_iwres_ipred <- function(
  ctr,...){
  pmx_plot_generic(ctr,"iwres_ipred",defaults_iwres_ipred,...)
}


# abs IWRES vs IPRED plot --------------------------------------------------------------


#' |IWRES| vs IPRED plot
#' @return ggplot2 object
#' @export
pmx_plot_abs_iwres_ipred <- function(
  ctr,...){
  defaults_abs_iwres_ipred$trans <- "abs_y"
  pmx_plot_generic(ctr,"abs_iwres_ipred",defaults_abs_iwres_ipred,...)
}



# IWRES vs TIME plot --------------------------------------------------------------


#' IWRES vs TIME plot
#' @return ggplot2 object
#' @export
pmx_plot_iwres_time <- function(ctr,...){
  pmx_plot_generic(ctr,"iwres_time",defaults_iwres_time,...)
}


# NPDE vs TIME plot --------------------------------------------------------------

#' NPDE vs TIME plot
#' @return ggplot2 object
#' @export
pmx_plot_npde_time <- function(
  ctr, ...){
  pmx_plot_generic(ctr,"npde_time",defaults_npde_time,...)
}

# NPDE vs PRED plot --------------------------------------------------------------

#' NPDE vs PRED plot
#' @return ggplot2 object
#' @export
pmx_plot_npde_pred<- function(
  ctr,
  ...){
  pmx_plot_generic(ctr,"npde_pred",defaults_npde_pred,...)
}



# Eta matrix plot --------------------------------------------------------------


#' Eta matrix plot
#' @return ggplot2 object
#' @export
pmx_plot_eta_matrix <-  function(ctr,...){
  pmx_plot_generic(ctr,"eta_matrix",defaults_eta_matrix,...)
}



# Distribution boxplot --------------------------------------------------------------

#' Distribution boxplot
#' @return ggplot2 object
#' @export
pmx_plot_ebe_box <- 
  function(ctr,
           ...){
    pmx_plot_generic(ctr,"ebe_box",defaults_eta_box,...)
  }

# Distribution histogram plot --------------------------------------------------------------


#' Distribution histogram plot
#' @return ggplot2 object
#' @export
pmx_plot_ebe_hist <- 
  function(
    ctr,
    ...){
    pmx_plot_generic(ctr,"ebe_hist",defaults_ebe_hist,...)
  }

# Individual plot --------------------------------------------------------------



#' Individual plot
#' @return ggplot2 object
#' @export
pmx_plot_individual <- 
  function(
    ctr,
    npage,
    ...){
    stopifnot(is_pmxclass(ctr))
    if(!"indiv" %in% (ctr %>% plot_names))return(NULL)
    
    cctr <- pmx_copy(ctr) 
    
    cctr %>%
      pmx_update(
        "indiv",
        ...
      )
    p <- cctr %>%  get_plot("indiv",npage)
    rm(cctr)
    p
  }

# eta cats plot --------------------------------------------------------------

#' Relationships between (ETA) and categorical covariates
#' @return ggplot2 object
#' @export
pmx_plot_eta_cats <- 
  function(ctr,
           ...){
    
    pmx_plot_generic(ctr,"eta_cats",list(),...)
    
  }

# eta conts plot --------------------------------------------------------------

#' Relationships between (ETA) and continuous covariates
#' @inheritDotParams pmx_update
#' @export
pmx_plot_eta_conts <- 
  function(ctr,
           ...){
    pmx_plot_generic(ctr,"eta_conts",list(),...)
  }



# Quantile-quantile plot of IWRES --------------------------------------------------------------

#' Quantile-quantile plot of IWRES
#' @return ggplot2 plot
#' @export
pmx_plot_iwres_qq <- 
  function(ctr,
           ...){
    pmx_plot_generic(ctr,"iwres_qq",defaults_iwres_qq,...)
  }


#' Quantile-quantile plot of NPDE
#' @return ggplot2 plot
#' @export
pmx_plot_npde_qq <- 
  function(ctr,
           ...){
    
    pmx_plot_generic(ctr,"npde_qq",defaults_npde_qq,...)
    
  }


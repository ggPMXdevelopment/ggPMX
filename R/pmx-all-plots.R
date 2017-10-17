

# DV vs PRED plot --------------------------------------------------------------

#' DV vs PRED plot
#' @inherit residual
#' @inheritDotParams pmx_update filter:trans
#' @export
pmx_plot_dv_pred <- function(
  ctr,
  labels = list(
    title="DV vs PRED",
    subtitle = "",
    x = "PRED",
    y = "DV"
  ), 
  point = list(shape = 1, color = "black", size = 1), 
  add_hline=FALSE, 
  dname="predictions",
  has.smooth=TRUE,
  smooth=list(se=FALSE,color="red",linetype=1),
  has.identity_line=TRUE,
  identity_line=list(intercept=0,color="blue"),
  ...){
  
  
  stopifnot(is_pmxclass(ctr))
  cctr <- pmx_copy(ctr) 
  assert_that(is_list_or_null(labels))
  assert_that(is_string_or_null(dname))
  assert_that(is.list(point))
  
  cctr %>%
    pmx_update(
      "dv_pred",
      labels=labels,
      point=point,
      add_hline=add_hline,
      dname=dname,
      has.smooth=has.smooth,
      smooth=smooth,
      has.identity_line=has.identity_line,
      identity_line=identity_line,
      ...
    )
  
  p <- cctr %>%  get_plot("dv_pred")
  rm(cctr)
  p
}


# IWRES vs IPRED plot --------------------------------------------------------------


#' IWRES vs IPRED plot
#' @inheritParams residual
#' @inheritDotParams pmx_update filter:trans
#' @export
#' @return ggplot2 plot
#' @export
pmx_plot_iwres_ipred <- function(
  ctr,
  labels = list(
    title="IWRES vs IPRED",
    subtitle = "",
    x = "IWRES",
    y = "Individual predictions"
  ), 
  point = list(shape = 1, color = "black", size = 1), 
  add_hline=TRUE, 
  dname="predictions",
  has.smooth=TRUE,
  smooth=list(se=FALSE,color="red",linetype=1),
  ...){
  
  
  stopifnot(is_pmxclass(ctr))
  cctr <- pmx_copy(ctr) 
  assert_that(is_list_or_null(labels))
  assert_that(is_string_or_null(dname))
  assert_that(is.list(point))
  
  cctr %>%
    pmx_update(
      "iwres_ipred",
      labels=labels,
      point=point,
      add_hline=add_hline,
      dname=dname,
      has.smooth=has.smooth,
      smooth=smooth,
      ...
    )
  
  p <- cctr %>%  get_plot("iwres_ipred")
  rm(cctr)
  p
}

# IWRES vs TIME plot --------------------------------------------------------------


#' IWRES vs TIME plot
#' @inheritParams residual
#' @inheritDotParams pmx_update filter:trans
#' @export
#' @return ggplot2 plot
#' @export
pmx_plot_iwres_time <- function(
  ctr,
  labels = list(
    title="IWRES vs TIME",
    subtitle = "",
    x = "TIME",
    y = "Individual predictions"
  ), 
  point = list(shape = 1, color = "black", size = 1), 
  add_hline=TRUE, 
  dname="predictions",
  has.smooth=TRUE,
  smooth=list(se=FALSE,color="red",linetype=1),
  ...){
  
  
  stopifnot(is_pmxclass(ctr))
  cctr <- pmx_copy(ctr) 
  assert_that(is_list_or_null(labels))
  assert_that(is_string_or_null(dname))
  assert_that(is.list(point))
  
  cctr %>%
    pmx_update(
      "iwres_time",
      labels=labels,
      point=point,
      add_hline=add_hline,
      dname=dname,
      has.smooth=has.smooth,
      smooth=smooth
    )
  
  p <- cctr %>%  get_plot("iwres_time")
  rm(cctr)
  p
}

# Eta matrix plot --------------------------------------------------------------


#' Eta matrix plot
#' @param ctr the controller
#' @inheritParams eta_pairs
#' @return ggplot2 pbject
#' @export
pmx_plot_eta_matrix <- 
  function(ctr,
           title= "Correlations of random effects",
           dname="eta",
           type.eta="mode",
           text_color="black",...){
  
  stopifnot(is_pmxclass(ctr))
  cctr <- pmx_copy(ctr) 
  
  cctr %>%
    pmx_update(
      "eta_matrix",
      title=title,
      dname=dname,
      type.eta=type.eta,
      text_color=text_color,
      ...)
  
  p <- cctr %>%  get_plot("eta_matrix")
  rm(cctr)
  p
  
}

# Distribution boxplot --------------------------------------------------------------

#' Distribution boxplot
#' @inherit distrib
#' @inheritDotParams pmx_update
#' @export
pmx_plot_ebe_box <- 
  function(ctr,
           labels,
           has.jitter = FALSE,
           jitter = list(shape = 1, color = "grey50", width = 0.1),
           has.shrink = TRUE,
           shrink=list(fun="sd",size=4,color="black"),
           dname = NULL,
           ...){
    stopifnot(is_pmxclass(ctr))
    cctr <- pmx_copy(ctr) 
    
    assert_that(is_logical(has.jitter))
    assert_that(is_list(jitter))
    assert_that(is_logical(has.shrink))
    assert_that(is_list(shrink))
    assert_that(is_string_or_null(dname))
    if(is.null(dname)) dname <- "eta"
    
    
    cctr %>%
      pmx_update(
        "ebe_box",
        type="box",
        has.jitter = has.jitter,
        jitter = jitter,
        has.shrink = has.shrink,
        shrink=shrink,
        dname = dname,
        ...
      )
    
    p <- cctr %>%  get_plot("ebe_box")
    rm(cctr)
    p
  }

# Distribution histogram plot --------------------------------------------------------------


#' Distribution histogram plot
#' @inherit distrib
#' @inheritDotParams pmx_update
#' @export
pmx_plot_ebe_hist <- 
  function(
    ctr,
    labels,
    facets = list(scales = "free_y", nrow = 3),
    has.shrink = TRUE,
    shrink=list(
      fun="sd",size=4,color="black",
      x_=-Inf,y_=Inf,
      hjust=-0.5,vjust=2),
    dname = NULL,
    ...){
    stopifnot(is_pmxclass(ctr))
    cctr <- pmx_copy(ctr) 
    
    assert_that(is_list(facets))
    assert_that(is_logical(has.shrink))
    assert_that(is_list(shrink))
    assert_that(is_string_or_null(dname))
    if(is.null(dname)) dname <- "eta"
    
    
    cctr %>%
      pmx_update(
        "ebe_hist",
        type="hist",
        facets = facets,
        has.shrink = has.shrink,
        shrink=shrink,
        dname = dname,
        ...
      )
    
    p <- cctr %>%  get_plot("ebe_hist")
    rm(cctr)
    p
  }

# Individual plot --------------------------------------------------------------



#' Individual plot
#' @inherit individual
#' @inheritDotParams pmx_update
#' @export
pmx_plot_individual <- 
  function(
    ctr,
    npage,
    ...){
    stopifnot(is_pmxclass(ctr))
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
#' @inheritDotParams pmx_update
#' @export
pmx_plot_eta_cats <- 
  function(ctr,
           ...){
    
    stopifnot(is_pmxclass(ctr))
    cctr <- pmx_copy(ctr) 
    
    cctr %>%
      pmx_update(
        "eta_cats",
        ...
      )
    
    p <- cctr %>%  get_plot("eta_cats")
    rm(cctr)
    p
    
  }

# eta conts plot --------------------------------------------------------------

#' Relationships between (ETA) and continuous covariates
#' @inheritDotParams pmx_update
#' @export
pmx_plot_eta_conts <- 
  function(ctr,
           ...){
    
    stopifnot(is_pmxclass(ctr))
    cctr <- pmx_copy(ctr) 
    
    cctr %>%
      pmx_update(
        "eta_conts",
        ...
      )
    
    p <- cctr %>%  get_plot("eta_conts")
    rm(cctr)
    p
    
  }



# Quantile-quantile plot of IWRES --------------------------------------------------------------

#' Quantile-quantile plot of IWRES
#' @inheritDotParams pmx_update
#' @export
pmx_plot_iwres_qq <- 
  function(ctr,
           labels = list(
             title="",
             subtitle = "",
             x = "Standard Normal Quantiles",
             y = "IWRES Quantiles"
           ),
           ...){
    
    stopifnot(is_pmxclass(ctr))
    cctr <- pmx_copy(ctr) 
    
    cctr %>%
      pmx_update(
        "iwres_qq",
        labels=labels,
        ...
      )
    
    p <- cctr %>%  get_plot("iwres_qq")
    rm(cctr)
    p
    
  }

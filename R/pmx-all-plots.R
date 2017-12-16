pmx_plot_generic <-
  function(ctr, pname, defaults_, ...) {
    stopifnot(is_pmxclass(ctr))
    if (!pname %in% (ctr %>% plot_names())) return(NULL)
    cctr <- pmx_copy(ctr)

    params <- c(
      ctr = cctr,
      pname = pname,
      l_left_join(defaults_, list(...))
    )
    do.call("pmx_update", params)
    p <- cctr %>% get_plot(pname)
    rm(cctr)
    p
  }


lang_to_expr <- function(params) {
  if ("filter" %in% names(params)) {
    if (is.language(params$filter)) {
      params$filter <- deparse(params$filter)
    }
  }
  params
}

wrap_pmx_plot_generic <-
  function(ctr, pname, params, defaults_) {
    params$ctr <- ctr
    params$pname <- pname
    params <- lang_to_expr(params)
    params$defaults_ <- get_plot_defaults(pname)
    do.call(pmx_plot_generic, params)
  }

# DV vs PRED plot --------------------------------------------------------------

#' DV vs PRED plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @example inst/examples/residual.R
#' @family residual
pmx_plot_dv_pred <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "dv_pred", params)
}


# DV vs IPRED plot --------------------------------------------------------------

#' DV vs IPRED plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @family residual
#' @example inst/examples/residual.R


pmx_plot_dv_ipred <- function(
                              ctr,
                              ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "dv_ipred", params)
}


# IWRES vs IPRED plot --------------------------------------------------------------


#' IWRES vs IPRED plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @family residual
#' @example inst/examples/residual.R

pmx_plot_iwres_ipred <- function(
                                 ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "iwres_ipred", params)
}


# abs IWRES vs IPRED plot --------------------------------------------------------------


#' |IWRES| vs IPRED plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @family residual
#' @example inst/examples/residual.R

pmx_plot_abs_iwres_ipred <- function(
                                     ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "abs_iwres_ipred", params)
}



# IWRES vs TIME plot --------------------------------------------------------------


#' IWRES vs TIME plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @family residual
#' @example inst/examples/residual.R

pmx_plot_iwres_time <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "iwres_time", params)
}


# NPDE vs TIME plot --------------------------------------------------------------

#' NPDE vs TIME plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @family residual
#' @example inst/examples/residual.R

pmx_plot_npde_time <- function(
                               ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "npde_time", params)
}
# NPDE vs PRED plot --------------------------------------------------------------

#' NPDE vs PRED plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{residual}} generic object for all residual (scatter) plots .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @family residual
#' @example inst/examples/residual.R

pmx_plot_npde_pred <- function(
                               ctr,
                               ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "npde_pred", params)
}


# Eta matrix plot --------------------------------------------------------------


#' Eta matrix plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{eta_pairs}} ggPMX internal function for eta matrix plot.
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @example inst/examples/eta_matrix.R
#' @export
pmx_plot_eta_matrix <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "eta_matrix", params)
}



# Distribution boxplot --------------------------------------------------------------

#' Distribution boxplot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{distrib}} generic object for distribution plots (histogram/boxplot).
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @example inst/examples/distribution.R

pmx_plot_ebe_box <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "ebe_box", params)
  }

# Distribution histogram plot --------------------------------------------------------------


#' Distribution histogram plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{distrib}} generic object for distribution plots (histogram/boxplot).
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @example inst/examples/distribution.R

pmx_plot_ebe_hist <-
  function(
           ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "ebe_hist", params)
  }

# Individual plot --------------------------------------------------------------



#' Individual plot
#' @param ctr pmx controller
#' @param npage \code{integer} page(s) to display
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{individual}} generic object for individual plots.
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @example inst/examples/individual.R
pmx_plot_individual <-
  function(
           ctr,
           npage=1,
           ...) {
    stopifnot(is_pmxclass(ctr))
    if (!"individual" %in% (ctr %>% plot_names())) return(NULL)
    cctr <- pmx_copy(ctr)
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    params <- lang_to_expr(params)
    params$pname <- "individual"
    params$ctr <- cctr
    do.call("pmx_update", params)
    p <- cctr %>% get_plot("individual", npage)
    rm(cctr)
    p
  }


# eta cats plot --------------------------------------------------------------

#' Relationships between (ETA) and categorical covariates
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{eta_cov}}  generic object for eta/covariates plots
#' \item \code{\link{pmx_update}} function.
#' }

#' @return ggplot2 object
#' @export
#' @example inst/examples/eta_cov.R
#' @family eta covariates
pmx_plot_eta_cats <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "eta_cats", params)
  }

# eta conts plot --------------------------------------------------------------

#' Relationships between (ETA) and continuous covariates
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} low level function grahical object
#' \item \code{\link{eta_cov}} generic object for eta/covariates plots.
#' \item \code{\link{pmx_update}} function.
#' }
#' @family eta covariates
#' @export
pmx_plot_eta_conts <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "eta_conts", params)
  }



# Quantile-quantile plot of IWRES --------------------------------------------------------------

#' Quantile-quantile plot of IWRES
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object.
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 plot
#' @export
pmx_plot_iwres_qq <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "iwres_qq", params)
  }


#' Quantile-quantile plot of NPDE
#' @return ggplot2 plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object
#' \item \code{\link{pmx_update}} function.
#' }
#' @export
pmx_plot_npde_qq <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "npde_qq", params)
  }

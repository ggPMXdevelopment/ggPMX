pmx_plot_generic <-
  function(ctr, pname, defaults_, ...) {
    stopifnot(is_pmxclass(ctr))
    if (!pname %in% (ctr %>% plot_names())) return(NULL)
    cctr <- pmx_copy(ctr, ...)

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
    params$defaults_ <- ctr$config$plots[[toupper(pname)]]
    if (!exists("bloq", params) && !is.null(ctr$bloq)) {
      params$defaults_[["bloq"]] <- ctr$bloq
    }

    pp <- do.call(pmx_plot_generic, params)
    if (ctr$footnote && !is.null(pp)) {
      ctr$enqueue_plot(pname)
      if (exists("footnote", params)) {
        footnote <- params$footnote
      } else {
        footnote <- ctr$plot_file_name
      }
      add_footnote(pp, footnote, ctr$save_dir)
    } else {
      pp
    }
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

pmx_plot_eta_box <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "eta_box", params)
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

pmx_plot_eta_hist <-
  function(
           ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "eta_hist", params)
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




#' Genereic pmx plot
#'
#' @param ctr \code{pmxClass} pmx controller
#' @param pname plot name.
#' @param ...  others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object
#' \item \code{\link{pmx_update}} function.
#' }
#' @export
#' @family plot_pmx

pmx_plot <- function(ctr, pname, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, pname, params)
}

#' Genereic pmx stratified plot
#'
#' @param ctr \code{pmxClass} pmx controller
#' @param pname plot name
#' @param cats list of categorical variables. By default all of them
#' @param chunk chunk name
#' @param print \code{logical} if TRUE print plots otherwise the list of plots is returned
#' @param ...  others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object
#' \item \code{\link{pmx_update}} function.
#' }
#' @export
#'

pmx_plot_cats <- function(ctr, pname, cats, chunk="", print=TRUE, ...) {
  sp <- list()
  if (missing(cats)) cats <- ctr %>% get_cats()
  if (length(cats) == 0 || cats == "") {
    invisible(return(NULL))
  }
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  for (i in seq_along(cats))
  {
    params$strat.facet <- cats[[i]]
    p <- wrap_pmx_plot_generic(ctr, pname, params)
    sp[[i]] <- p
  }
  if (length(sp) > 0 && print) invisible(lapply(sp, print))
  invisible(sp)
}


#' Quantile-quantile plot of eta variables
#' @return ggplot2 plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object
#' \item \code{\link{pmx_update}} function.
#' }
#' @export
pmx_plot_eta_qq <-
  function(ctr,
           ...) {
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    wrap_pmx_plot_generic(ctr, "eta_qq", params)
  }




#' Register plot
#'
#' @param ctr \code{pmxClass} controller
#' @param pp \code{ggplot2} plot
#' @param pname \code{character} plot nme
#'
#' @export
pmx_register_plot <-
  function(ctr, pp, pname=NULL) {
    if (ctr$footnote) {
      if (is.null(pname)) pname <- "extra-plot"
      ctr$enqueue_plot(pname)
      footnote <- paste0(pname, "-", ctr$report_n)
      add_footnote(pp, footnote, ctr$save_dir)
    } else {
      pp
    }
  }


# VPC plot --------------------------------------------------------------

#' VPC plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_vpc}} vpc object .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @example inst/examples/vpc.R
#' @family vpc
pmx_plot_vpc <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  params$is.smooth <- FALSE
  wrap_pmx_plot_generic(ctr, "pmx_vpc", params)
}



# IWRES density plot --------------------------------------------------------------

#' IRES density plot
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_dens}} vpc object .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
pmx_plot_iwres_dens <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  params$is.smooth <- FALSE
  wrap_pmx_plot_generic(ctr, "iwres_dens", params)
}

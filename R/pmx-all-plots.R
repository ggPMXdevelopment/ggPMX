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




add_footnote <- function(pp, pname, save_dir) {
  plot_file <- file.path(save_dir, "ggpmx_GOF", pname)
  footnote <- sprintf("Source: %s", plot_file)
  ## message("footnote is :" , footnote)
  if(nchar(footnote)>45){
    fns <- strsplit(footnote,"/")[[1]]
    term1 <- do.call(file.path,as.list(fns[cumsum(nchar(fns))<45]))
    term2 <- do.call(file.path,as.list(fns[cumsum(nchar(fns))>=45]))
    footnote <- paste(paste0(term1,"/"),term2,sep="\n")
    
    
  }
  pp <- pp + labs(caption = footnote)
  pp
}
#' Individual plot
#' @param ctr pmx controller
#' @param npage \code{integer} page(s) to display , set npage to NULL
#' if you want to have all the individual plots
#' @param print \code{logical} if TRUE print directly the result to the current
#' device. This is useful to get rid of list indices output when generating
#' a rprot with a list of individual plots.
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
           print=FALSE,
           ...) {
    stopifnot(is_pmxclass(ctr))
    if (!"individual" %in% (ctr %>% plot_names())) return(NULL)
    cctr <- pmx_copy(ctr, ...)
    params <- as.list(match.call(expand.dots = TRUE))[-1]
    params <- lang_to_expr(params)
    defaults_ <- ctr$config$plots[[toupper("individual")]]
    params <- l_left_join(defaults_, params)
    params$pname <- "individual"
    params$ctr <- cctr


    do.call("pmx_update", params)
    p <- if (is.null(npage)) {
      cctr %>% get_plot("individual")
    } else {
      cctr %>% get_plot("individual", npage)
    }

    cctr %>% pmx_warnings("MISSING_FINEGRID")



    if (cctr$footnote) {
      if (!inherits(p, "ggplot")) {
        p <- Map(
          function(p, id) {
            ctr$enqueue_plot("indiv")
            add_footnote(p, ctr$plot_file_name, cctr$save_dir)
          },
          p, seq_along(p)
        )
      } else {
        ctr$enqueue_plot("indiv")
        p <- add_footnote(p, ctr$plot_file_name, cctr$save_dir)
      }
    }

    rm(cctr)

    if (print) {
      if (is.list(p)) invisible(lapply(p, print)) else p
    } else {
      p
    }
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
#' @param pname plot name
#' @param ...  others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical paramters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object
#' \item \code{\link{pmx_update}} function.
#' }
#' @export
#'
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
    ctr %>% pmx_plot("eta_qq", ...)
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
      footnote <- paste0(pname, ctr$report_n)
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
#' \item \code{\link{vpc}} vpc object .
#' \item \code{\link{pmx_update}} function.
#' }
#' @return ggplot2 object
#' @export
#' @example inst/examples/vpc.R
#' @family vpc
pmx_plot_vpc <- function(ctr, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  wrap_pmx_plot_generic(ctr, "vpc", params)
}


pmx_plot_generic <-
  function(ctr, pname, defaults_, ...) {
    stopifnot(is_pmxclass(ctr))

    if (!pname %in% (ctr %>% plot_names())) {
      return(NULL)
    }
    
    cctr <- pmx_copy(ctr, ...)
    if("shrink" %in% names(list(...))) {
      if(!"fun" %in% names(list(...)[["shrink"]])) {
        stop("Shrink argument (list) does not contain an element named 'fun'")
      }
    }
    len_list <- length(list(...))
    if (len_list != 0 ||
          (!is.null(ctr[["bloq"]])) ||
          (!is.null(ctr[["settings"]]))) {
      #if params were set through pmxgpar, then modify default params with pmxgpar
      if ("pmxgpar" %in% names(list(...))) 
        plot_params <- utils::modifyList(defaults_, list(...)[["pmxgpar"]])
      else if (len_list != 0) {
        plot_params <- l_left_join(defaults_, list(...))
      } else {
        plot_params <- NULL
      }
        
      params <- c(
        ctr = cctr,
        pname = pname,
        plot_params
      )
      do.call("pmx_update", params)
      p <- cctr %>% get_plot(pname)
    }
    else
      p <- ctr %>% get_plot(pname)
    rm(cctr)
    p
  }


lang_to_expr <-
  function(params) {
    if ("filter" %in% names(params)) {
      if (is.language(params$filter)) {
        params$filter <- deparse(params$filter)
      }
    }
    params
  }


#' Try to evaluate a symbol in the parent frame (on error return the symbol)
#' @param x any object
#' @returns evaluated symbol or the symbol in case of evaluation error
eval_sym_parent_env <- function(x) {
  if(is.symbol(x)){
    tryCatch(eval(x, envir = parent.frame(1L)), error=function(e) x)
  } else {
    x
  }
}


#' Get list of parameters based on parameters used in own function call
#' @return list of parameters based on parameters used in own function call
#' @noRd
get_params_from_call <- function () {
  . <- NULL # R check hack....
  match.call(
    definition=sys.function(sys.parent()),
    call=sys.call(sys.parent()),
    envir=parent.frame(2L),
    expand.dots=TRUE
  ) %>%
  .[-1] %>%
  # Make sure any symbols are evaluated
  lapply(eval_sym_parent_env) %>%
  lang_to_expr
}


wrap_pmx_plot_generic <-
  function(ctr, pname, params, defaults_) {
    params$ctr <- ctr
    params$pname <- pname
    params <- lang_to_expr(params)
    params$defaults_ <- ctr$config$plots[[toupper(pname)]]
    params[["custom_title"]] <- is.character(params[["labels"]][["title"]])
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





#' Register plot  (Jun2025, Alex: I believe it doesn't work at all)
#'
#' @param ctr \code{pmxClass} controller
#' @param pp \code{ggplot2} plot
#' @param pname \code{character} plot nme
#' @return NULL
#' @export
pmx_register_plot <-
  function(ctr, pp, pname = NULL) {
    if (ctr$footnote) {
      if (is.null(pname)) pname <- "extra-plot"
      ctr$enqueue_plot(pname)
      footnote <- paste0(pname, "-", ctr$report_n)
      add_footnote(pp, footnote, ctr$save_dir)
    } else {
      pp
    }
  }


#' Generic pmx plot
#'
#' @param ctr \code{pmxClass} pmx controller
#' @param pname plot name
#' @param ...  others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object
#' \item \code{\link{pmx_update}} function.
#' }
#' @returns ggplot2 object
#' @export
#' 
#'
pmx_plot <- function(ctr, pname, ...) {
  params <- get_params_from_call()
  wrap_pmx_plot_generic(ctr, pname, params)
}



#' Generic pmx stratified plot
#'
#' @param ctr \code{pmxClass} pmx controller
#' @param pname plot name
#' @param cats list of categorical variables. By default all of them
#' @param chunk chunk name
#' @param print \code{logical} if TRUE print plots otherwise the list of plots is returned
#' @param ...  others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{pmx_qq}} quantile-quantile plot object
#' \item \code{\link{pmx_update}} function.
#' }
#' @returns ggplot2 object(s)
#' @export
#'

pmx_plot_cats <- function(ctr, pname, cats, chunk = "", print = TRUE, ...) {
  sp <- list()
  if (missing(cats)) cats <- ctr %>% get_cats()
  if (length(cats) == 0 || cats == "") {
    invisible(return(NULL))
  }

  params <- get_params_from_call()

  for (i in seq_along(cats))
  {
    params$strat.facet <- cats[[i]]
    p <- wrap_pmx_plot_generic(ctr, pname, params)
    sp[[i]] <- p
  }
  if (length(sp) > 0 && print) invisible(lapply(sp, print))
  invisible(sp)
}

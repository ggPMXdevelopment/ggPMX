pmx_plot_generic <-
  function(ctr, pname, defaults_, ...) {
    stopifnot(is_pmxclass(ctr))
    if (!pname %in% (ctr %>% plot_names())) {
      return(NULL)
    }
    cctr <- pmx_copy(ctr, ...)

    if (length(list(...)) != 0){
         params <- c(
                     ctr = cctr,
                     pname = pname,
                     l_left_join(defaults_, list(...))
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





#' Register plot
#'
#' @param ctr \code{pmxClass} controller
#' @param pp \code{ggplot2} plot
#' @param pname \code{character} plot nme
#'
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
#' @export
#'
pmx_plot <- function(ctr, pname, ...) {
  params <- as.list(match.call(expand.dots = TRUE))[-1]
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
#' @export
#'

pmx_plot_cats <- function(ctr, pname, cats, chunk = "", print = TRUE, ...) {
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

#' Compute Shrinkage
#'
#' @param ctr \code{pmxClass} controller object
#' @param fun \code{character} can be sd or var
#' @param filter optional filter which will be applied to plotting data
#' @param strat.facet \code{formula} optional stratification parameter
#' @param strat.color \code{character} optional stratification parameter
#' @param ... others parameters not used for the moment

#'
#' @return \code{data.table}
#' @export
#'
pmx_comp_shrink <-
  function(ctr, fun = c("sd", "var"), strat.facet, strat.color, filter, ...) {
    VAR <- FUN <- PARAM <- EFFECT <- NULL
    VALUE <- OMEGA <- EBE <- NULL
    stopifnot(is_pmxclass(ctr))
    ## cherch variable and missing
    fun <- match.arg(fun)
    if (missing(strat.facet)) strat.facet <- NULL
    if (missing(strat.color)) strat.color <- NULL
    if (missing(filter)) filter <- NULL

    cctr <- pmx_copy(ctr)




    eta <- cctr %>% get_data("eta")

    ## filtering
    if (!is.null(substitute(filter))) {
      if (!any(grepl("filter", substitute(filter)))) {
        filter <- deparse(substitute(filter))
        filter <- local_filter(filter)
        eta <- filter(eta)
      } else {
        if (!is.null(filter)) eta <- filter(eta)
      }
    }

    ## stratification
    grp <- as.character(unlist(lapply(strat.facet, as.list)))
    grp <- unique(intersect(c(grp, strat.color), names(eta)))
    if (exists("FUN", eta)) eta <- eta[grepl("mode", FUN)]
    cols <- c("ID", "EFFECT", "VALUE", grp)
    eta <- unique(eta[, cols, with = FALSE])



    ## merge data

    setnames(eta, "VALUE", "EBE")
    omega <- cctr %>% get_data("omega")
    if (is.null(omega)) {
      estimates <- cctr %>% get_data("estimates")
      if (is.null(estimates)) {
        message("No estimates data , we can not compute shrinkage")
        return(NULL)
      }
      omega <- estimates[grepl("omega", PARAM)]
      omega[, EFFECT := gsub("(^ +)?omega_", "", PARAM)]
      omega <- omega [, list(EFFECT, OMEGA = VALUE)]
    }

    dx <- merge(eta, omega, by = "EFFECT")

    by <- setdiff(names(dx), c("EBE", "ID"))


    dx[, {
      coef <- if (fun == "sd") OMEGA else OMEGA^2
      shrink <- 1 - get(fun)(EBE) / coef
      list(SHRINK = shrink, POS = max(EBE) / 2, FUN = fun)
    }, by]
  }

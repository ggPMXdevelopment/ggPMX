

#' Creates vpc bins
#'
#' @param style \code{character} style	chosen on of the:\cr
#'  "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust" or "jenks"
#' @param within_strat \code{logical} if TRUE compute the bining for each strat level. \cr
#' By default t is false and bining are equal for all stratifications levels.
#' @param ... other classInt::classIntervals parameters excpet \code{style} and \code{n}
#'
#' @export
#' @details
#' This is a warraper to
#' @family vpc

pmx_vpc_bin <-
  function(style, within_strat=FALSE, ...) {
    if (missing(style)) return(NULL)
    rr <- as.list(match.call()[-1])
    rr
  }





#' Sets vpc observation layer
#'
#' @param show \code{logical} if TRUE show observation points
#' @param color \code{character} Color of the observed endpoint values. Default: "#000000".
#' @param size \code{numeric} Size of the observed endpoint values. Default: 1.
#' @param alpha \code{numeric} Transparency of the observed endpoint values. Default: 0.7.
#' @param shape \code{numeric} Shape of the observed endpoint values. Default: 1.
#'
#' @export
#' @family vpc
pmx_vpc_obs <-
  function(show=TRUE,
           color ="#000000",
           size =1,
           alpha = 0.7,
           shape = 1) {
    if (show) {
      structure(
        list(
          color = color,
          size = size,
          alpha = alpha,
          shape = shape
        ),
        class = c("pmx_vpc_obs", "list")
      )
    }
  }


#' Sets vpc percentile layer
#'
#' @param show  \code{charcater} how lines are displayed:
#' \itemize{
#' \item {\strong{show=all}} {lines will be displayed for each of
#' the 3 percentiles. }
#' \item {\strong{show=median}} {Show only median line.}
#' }

#' @param interval \code{numeric} quantiles values default
#' to \code{c(.05,.95)}
#' @param median \code{list} containing: \cr
#' \itemize{
#' \item {\strong{color}} {\code{charcater}  Color of the median percentile line. Default: "#000000". }
#' \item {\strong{size}} {\code{numeric}  Thickness of the median percentile line. Default: 1.}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the median percentile line. Default: 0.7.}
#' \item {\strong{linetype}} {\code{charcater} Linetype of the median percentile line. Default: "solid".}
#' }

#' @param extreme \code{list} containing: \cr
#' \itemize{
#' \item {\strong{color}} {\code{charcater} Color of the median percentile line. Default: "#000000". }
#' \item {\strong{size}} {\code{numeric} Thickness of the median percentile line. Default: 1.}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the median percentile line. Default: 0.7.}
#' \item {\strong{linetype}} {\code{charcater} Linetype of the median percentile line. Default: "solid"}
#' }
#'
#' @family vpc
#' @export
pmx_vpc_pi <-
  function(show = c("all", "median"),
           interval=c(.05, .95),
           median=list(color = "#000000", size = 1, alpha = 0.7, linetype = "solid"),
           extreme=list(color = "#000000", size = 1, alpha = 0.7, linetype = "solid")) {
    show <- match.arg(show)
    median_default <- list(color = "#000000", size = 1, alpha = 0.7, linetype = "solid")
    extreme_default <- list(color = "#000000", size = 1, alpha = 0.7, linetype = "solid")

    median <- if (!missing(median)) {
      l_left_join(median_default, median)
    } else {
      median_default
    }
    extreme <- if (!missing(extreme)) {
      l_left_join(extreme_default, extreme)
    } else {
      extreme_default
    }
    structure(
      list(
        show = show,
        probs = interval,
        median = median,
        extreme = extreme
      ),
      class = c("pmx_vpc_pi", "list")
    )
  }



#' Sets vpc confidence interval layer

#' @param show  \code{charcater} how areas are displayed:
#' \itemize{
#' \item {\strong{show="all"}} {areas will be displayed for each of the 3 percentiles. }
#' \item {\strong{show="median"}} {Show only median area.}
#' }

#' @param interval \code{numeric} quantiles values default to \code{c(.05,.95)}
#' @param method \code{charcater} which areas are displayed:
#' \itemize{
#' \item {\strong{method="ribbon"}} {areas are ribbons.}
#' \item {\strong{method="rectangle"}} {ares are horizontal rectangles.}
#' }

#' @param median \code{list} containing: \cr
#' \itemize{
#' \item {\strong{fill}} { \code{character} Color of the area representing the CI for the median. Default: "#3388cc".}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the area representing the PI for the median. Default=0.3.}
#' }

#' @param extreme \code{list} containing: \cr
#' \itemize{
#' \item {\strong{fill}} {\code{character} Color of the area representing the CI for the extreme percentiles. Default: "#3388cc".}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the area representing the PI for the extreme percentiles. Default=0.3.}
#' }
#'
#' @export

#' @family vpc
pmx_vpc_ci <-
  function(show = c("all", "median"),
           interval=c(.05, .95),
           method = c("ribbon", "rectangle"),
           median=list(fill = "#3388cc", alpha = 0.3),
           extreme=list(fill = "#3388cc", alpha = 0.3)) {
    show <- match.arg(show)
    method <- match.arg(method)
    median_default <- list(fill = "#3388cc", alpha = 0.3)
    extreme_default <- list(fill = "#3388cc", alpha = 0.3)
    median <- if (!missing(median)) {
      l_left_join(median_default, median)
    } else {
      median_default
    }
    extreme <- if (!missing(extreme)) {
      l_left_join(extreme_default, extreme)
    } else {
      extreme_default
    }
    structure(
      list(
        show = show,
        method = method,
        probs = interval,
        median = median,
        extreme = extreme
      ),
      class = c("pmx_vpc_ci", "list")
    )
  }


#' Sets vpc rug layer
#'
#' @param show  \code{logical} If TRUE show bin separators
#' @param color \code{character} Color of the rug. Default: "#000000".
#' @param size  \code{numeric} Thickness of the rug. Default: 1.
#' @param alpha  \code{numeric} Transparency of the rug. Default: 0.7.
#'
#' @details
#'
#' When the vpc confidence interval layer method  is rectangles we don't show rug separators.

#'
#' @export
#'
#' @family vpc
pmx_vpc_rug <-
  function(show=TRUE,
           color = "#000000",
           size = 1,
           alpha =0.7) {
    if (show) {
      structure(
        list(
          color = color,
          size = size,
          alpha = alpha
        ),
        class = c("pmx_vpc_rug", "list")
      )
    }
  }










quantile_dt <-
  function(dx, grp="time", ind="y", probs=c(.05, .95), prefix="p", wide=FALSE) {
    percentile <- NULL
    probs <- sort(unique(c(0.5, probs)))
    fmt <- ifelse(probs < .1, paste0(prefix, "0%1.f"), paste0(prefix, "%1.f"))
    probs.n <- sprintf(fmt, probs * 100)
    if (wide) {
      dd <- dx[, as.list(stats::quantile(get(ind), probs = probs, na.rm = TRUE)), grp]
      setnames(dd, grep("%", names(dd)), probs.n)
    } else {
      ds <- dx[, stats::quantile(get(ind), probs = probs, na.rm = TRUE), grp]
      ds[, percentile := rep(probs.n, .N / length(probs))]
      setnames(ds, "V1", "value")
    }
  }

vpc.data <-
  function(type = c("percentile", "scatter"),
           dobs,
           dsim,
           probs.pi,
           probs.ci,
           idv = "time",
           irun="stu",
           dv="y",
           strat = NULL,
           rug=NULL) {
    zmax <- zmin <- out_ <- value <- NULL
    bins <- unlist(unique(dobs[, idv, with = FALSE]))
    if (is.null(rug)) {
      rug <- data.frame(x = bins, y = NA_real_, stringsAsFactors = FALSE)
    }

    if (type == "percentile") {
      pi <- quantile_dt(dobs, probs = probs.pi, grp = c(idv, strat), ind = dv)
      res2 <- quantile_dt(dsim, probs = probs.pi, grp = c(irun, idv, strat), ind = dv)
      ci <- quantile_dt(
        res2, probs = probs.ci, grp = c("percentile", idv, strat),
        prefix = "CL", ind = "value", wide = TRUE
      )
    } else {
      pi <- quantile_dt(dsim, probs = probs.pi, grp = c(idv, strat), ind = dv)
    }

    res <- list(pi_dt = pi, rug_dt = rug)
    if (type == "percentile") {
      res$ci_dt <- ci
      out <- merge(ci, pi, by = c(idv, "percentile"))
      nn <- grep("CL", names(out), value = TRUE)[c(1, 3)]
      out[, out_ := value < get(nn[[1]]) | value > get(nn[[2]])]
      out[, zmax := pmax(get(nn[[2]]), value)]
      out[, zmin := pmin(get(nn[[1]]), value)]
      res$out <- out
    }
    res
  }

bin_idv <- function(idv, x) {
  brks <- do.call(classIntervals, append(list(var = idv), x$bin))$brks
  if (max(brks) >= max(idv)) brks[which.max(brks)] <- max(idv)
  if (min(brks) <= min(idv)) brks[which.min(brks)] <- min(idv)
  brks
}



find_interval <- function(x, vec, labels=NULL, ...) {
  levels <- seq_along(vec)

  vals <- findInterval(x, vec, rightmost.closed = TRUE, ...)
  if (!is.null(labels)) {
    as.numeric(as.character(factor(vals, levels = unique(vals), labels = labels)))
  } else {
    stats::ave(x, vals, FUN = stats::median)
  }
}



.vpc_x <- function(x, self) {
  if (x$ptype == "VPC") {
    x$dv <- self$dv
    idv <- self$sim[["idv"]]
    rug <- bin <- brks <- NULL
    if (!is.null(x$bin)) {
      if (!is.null(x$strat.facet) && !is.null(x$bin$within_strat) && x$bin$within_strat) {
        x$bin$within_strat <- NULL
        bins <- x$input[, list(brks = bin_idv(get(idv), x)), c(x$strat.facet)]

        x$input[, bin := {
          grp <- get(x$strat.facet)
          find_interval(get(idv), bins[get(x$strat.facet) == grp, brks])
        }, c(x$strat.facet)]
        x$dx[, bin := {
          grp <- get(x$strat.facet)
          find_interval(get(idv), bins[get(x$strat.facet) == grp, brks])
        }, c(x$strat.facet)]
      } else {
        rugs <- x$input[, bin_idv(get(idv), x)]
        x$input[, bin := find_interval(get(idv), rugs)]
        x$dx[, bin := find_interval(get(idv), rugs, labels = unique(x$input[, bin]))]
        rug <- data.frame(x = rugs, y = NA_real_, stringsAsFactors = FALSE)
      }
    }
    res <- vpc.data(
      x[["type"]],
      x$input,
      x$dx,
      x$pi$probs,
      x$ci$probs,
      idv = if (!is.null(x$bin)) "bin" else self$sim[["idv"]],
      irun = self$sim[["irun"]],
      dv = self$dv,
      strat = x$strat.facet,
      rug = rug
    )
    old_class <- class(x)
    x$db <- res
    class(x) <- old_class
    x
  } else {
    x
  }
}




vpc.pi_line <- function(dt, left, geom) {
  mapping <- aes_string(group = "percentile", y = "value")
  right <- list(data = dt, mapping = mapping)
  do.call("geom_line", append(right, left))
}

.vpc.area <- function() {

  # out <- list(color="red")
  # out_layer <- if(!is.null(out)){
  #   params <- append(
  #     list(
  #       mapping = aes_string(group="percentile",y="value"),
  #       data=db$out[(out_)]),
  #     out)
  #   do.call(geom_point,params)
  # }
  # out_area <- list(fill="red",alpha=0.2)
  # out_layer_area_min <- if(!is.null(out_area)){
  #   ll <- list(
  #     mapping = aes_string(group="percentile",ymin="zmin",ymax=nn[[1]]),
  #     data=db$out
  #   )
  #   params <- append(ll,out_area)
  #   do.call(geom_ribbon,params)
  # }
  #
  # out_layer_area_max <- if(!is.null(out_area)){
  #   ll1 <- list(
  #     mapping = aes_string(group="percentile",ymax="zmax",ymin=nn[[2]]),
  #     data=db$out
  #   )
  #   params <- append(ll1,out_area)
  #   do.call(geom_ribbon,params)
  # }

  # list( out_layer , out_layer_area_min , out_layer_area_max )
}
vpc.plot <- function(x) {
  with(x, {
    pp <- ggplot(data = db$pi_dt, aes_string(x = if (!is.null(bin)) "bin" else idv))
    pi_med_layer <- if (!is.null(pi)) {
      vpc.pi_line(db$pi_dt[percentile == "p50"], pi$median)
    }
    pi_ext_layer <- if (!is.null(pi) && pi$show == "all") {
      vpc.pi_line(db$pi_dt[percentile != "p50"], pi$extreme)
    }
    obs_layer <- if (!is.null(obs)) {
      params <- append(
        list(
          mapping = aes_string(y = dv, x = idv),
          data = input
        ),
        obs
      )
      do.call(geom_point, params)
    }
    rug_layer <- if (!is.null(rug)) {
      params <- append(
        list(
          mapping = aes(x = x, y = y),
          sides = "t",
          data = db$rug_dt
        ),
        rug
      )

      do.call(geom_rug, params)
    }
    ci_med_layer <- if (!is.null(ci) && type == "percentile") {
      nn <- grep("CL", names(db$ci_dt), value = TRUE)[c(1, 3)]
      params <- append(
        list(
          data = db$ci_dt[percentile == "p50"],
          mapping = aes_string(ymin = nn[[1]], ymax = nn[[2]], group = "percentile")
        ),
        ci$median
      )
      do.call(geom_ribbon, params)
    }
    ci_ext_layer <- if (!is.null(ci) && type == "percentile" && ci$show == "all") {
      nn <- grep("CL", names(db$ci_dt), value = TRUE)[c(1, 3)]
      params <- append(
        list(
          data = db$ci_dt[percentile != "p50"],
          mapping = aes_string(ymin = nn[[1]], ymax = nn[[2]], group = "percentile")
        ),
        ci$extreme
      )
      do.call(geom_ribbon, params)
    }

    pp <- ggplot(data = db$pi_dt, aes_string(x = if (!is.null(bin)) "bin" else idv)) +
      obs_layer + pi_med_layer + pi_ext_layer +
      rug_layer + ci_med_layer + ci_ext_layer



    strat.facet <- x[["strat.facet"]]

    if (!is.null(strat.facet)) {
      if (is.character(strat.facet)) {
        strat.facet <- formula(paste0("~", strat.facet))
      }
      pp <- pp + do.call("facet_wrap", c(strat.facet, facets))
    }


    if (type == "percentile") {
      pp + labs(title = "Percentile VPC", subtitle = "(with observations)")
    } else {
      pp + labs(title = "Scatter VPC")
    }
  })
}

#' Creates vpc object
#'
#' @param type \code{charcater} can be either percentile or scatter
#' @param idv \code{chracater} individual variable
#' @param obs \code{pmx_vpc_obs} object observation layer \link{pmx_vpc_obs}
#' @param pi \code{pmx_vpc_pi} object percentile layer  \link{pmx_vpc_pi}
#' @param ci \code{pmx_vpc_ci} object confidence interval layer  \link{pmx_vpc_ci}
#' @param rug  \code{pmx_vpc_rug} object rug layer  \link{pmx_vpc_rug}
#' @param bin \code{pmx_vpc_bin} object  \link{pmx_vpc_bin}
#' @param labels \code{list} define title and axis labels
#' @param is.legend \code{logical} if TRUE add legend
#' @param dname added for compatibility with other ggPMX plots
#' @param facets is a list of parameters passed to facet_wrap in case of startification
#' @param ...  extra parameters passed to base graphical parameters
#'
#' @family vpc
#' @export
#'

pmx_vpc <- function(
                    type = c("percentile", "scatter"),
                    idv  ="TIME",
                    obs  = pmx_vpc_obs(),
                    pi =  pmx_vpc_pi(),
                    ci =  pmx_vpc_ci(),
                    rug = pmx_vpc_rug(),
                    bin = pmx_vpc_bin(),
                    labels = NULL,
                    facets = NULL,
                    is.legend=FALSE,
                    dname=NULL,
                    ...) {
  type <- match.arg(type)
  ## check args here

  structure(
    list(
      ptype = "VPC",
      strat = TRUE,
      idv = idv,
      dname = dname,
      labels = labels,
      is.legend = is.legend,
      type = type,
      facets = facets,
      obs = obs, pi = pi, ci = ci, rug = rug, bin = bin,
      gp = pmx_gpar(labels = labels, is.legend = is.legend, ...)
    ),
    class = c("pmx_vpc", "pmx_gpar")
  )
}


plot_pmx.pmx_vpc <- function(x, dx, ...) {
  db <- x$db
  if (!is.null(db)) p <- vpc.plot(x)
  plot_pmx(x$gp, p)
}

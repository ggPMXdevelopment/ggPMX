

#' Creates vpc bins
#'
#' @param style \code{character} style	chosen on of the:\cr
#'  "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust" or "jenks"
#' @param within_strat \code{logical} if TRUE compute the bining for each strat level. \cr
#' By default t is false and bining are equal for all stratifications levels.
#' @param seed \code{integer} used in \code{set.seed} call to ensure \cr
#'    reproducibility if style is "kmeans". Set to NULL if this \cr
#'    is not desired.
#' @param ... other classInt::classIntervals parameters excpet \code{style} and \code{n}
#'
#' @export
#' @details
#' This is a wrapper to the bin based VPC
#' @family vpc

pmx_vpc_bin <-
  function(style, within_strat = TRUE, seed = 42, ...) { #within strat = TRUE, as default in order to avoid bugs
    # Setting seed for random number generating, so plots are have reproducible binning.
    set.seed(seed)
    if (missing(style)) {
      return(NULL)
    }
    rr <- as.list(match.call()[-1])
    rr[!names(rr) == "seed"]
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
  function(show = TRUE,
           color = "#000000",
           size = 1,
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
#' the 3 percentiles. with a shaded area.}
#' \item {\strong{show=median}} {Show only median line.}
#' \item {\strong{show=area}} {Show only median line and the shaded area}

#' }

#' @param interval \code{numeric} quantiles values default
#' to \code{c(.05,.95)}
#' @param median \code{list} containing: \cr
#' \itemize{
#' \item {\strong{color}} {\code{charcater}  Color of the median percentile line. Default: "#000000". }
#' \item {\strong{linewidth}} {\code{numeric}  Thickness of the median percentile line. Default: 1.}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the median percentile line. Default: 0.7.}
#' \item {\strong{linetype}} {\code{charcater} Linetype of the median percentile line. Default: "solid".}
#' }

#' @param extreme \code{list} containing: \cr
#' \itemize{
#' \item {\strong{color}} {\code{charcater} Color of the median percentile line. Default: "#000000". }
#' \item {\strong{linewidth}} {\code{numeric} Thickness of the median percentile line. Default: 1.}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the median percentile line. Default: 0.7.}
#' \item {\strong{linetype}} {\code{charcater} Linetype of the median percentile line. Default: "solid"}
#' }
#' @param area \code{list} containing: \cr
#' \itemize{
#' \item {\strong{fill}} {\code{charcater}  Color of the shaded area. Default: "blue". }
#' \item {\strong{alpha}} {\code{numeric} Transparency of the sahded area. Default: 0.1.}
#' }
#'
#' @family vpc
#' @export
pmx_vpc_pi <-
  function(show = c("all", "median","area"),
           interval = c(.05, .95),
           median = list(color = "#000000", linewidth = 1, alpha = 0.7, linetype = "solid"),
           extreme = list(color = "#000000", linewidth = 1, alpha = 0.7, linetype = "dashed"),
           area = list(fill = "blue", alpha = 0.1)) {
    show <- match.arg(show)
    median_default <- list(color = "#000000", linewidth = 1, alpha = 0.7, linetype = "solid")
    extreme_default <- list(color = "#000000", linewidth = 1, alpha = 0.7, linetype = "dashed")
    area_default <- list(fill = "blue", alpha = 0.1)

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
    area <- if (!missing(area)) {
      l_left_join(area_default, area)
    } else {
      area_default
    }

    structure(
      list(
        show = show,
        probs = interval,
        median = median,
        extreme = extreme,
        area = area
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
           interval = c(.025, .975),
           method = c("ribbon", "rectangle"),
           median = list(fill = "red", alpha = 0.3),
           extreme = list(fill = "#3388cc", alpha = 0.3)) {
    show <- match.arg(show)
    method <- match.arg(method)
    median_default <- list(fill = "red", alpha = 0.3)
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
#' @param linewidth  \code{numeric} Thickness of the rug. Default: 1.
#' @param alpha  \code{numeric} Transparency of the rug. Default: 0.7.
#' @param size \code{numeric} Depreciated thickness of the rug. Default: 1.
#'
#' @details
#'
#' When the vpc confidence interval layer method  is rectangles we don't show rug separators.

#'
#' @export
#'
#' @family vpc
pmx_vpc_rug <-
  function(show = TRUE,
           color = "#000000",
           linewidth = 1,
           alpha = 0.7,
           size) {
    lifecycle::deprecate_soft("1.2.9", "pmx_vpc_rug(size)", I("use `linewidth=` instead of `size=`"))
    if (!missing(size)) linewidth <- size
    if (show) {
      structure(
        list(
          color = color,
          linewidth = linewidth,
          alpha = alpha
        ),
        class = c("pmx_vpc_rug", "list")
      )
    }
  }




quantile_dt <-
  function(dx, grp = "time", ind = "y", probs = c(.05, .95), prefix = "p", wide = FALSE) {
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
           irun = "stu",
           dv = "y",
           strat = NULL,
           rug = NULL) {
    zmax <- zmin <- out_ <- value <- percentile <- NULL
    bins <- unlist(unique(dobs[, idv, with = FALSE]))
    if (type == "percentile") {

      #allow for input e.g. pmx_plot_vpc(strat.facet = ~SEX)
      if(!is.character(strat)){
        strat <- as.character(strat)
        strat <- sub("~","",strat)
        strat <- strat[strat != ""]
      }

      pi <- quantile_dt(dobs, probs = probs.pi, grp = c(idv, strat), ind = dv)
      res2 <- quantile_dt(dsim, probs = probs.pi, grp = c(irun, idv, strat), ind = dv)
      ci <- quantile_dt(
        res2,
        probs = probs.ci, grp = c("percentile", idv, strat),
        prefix = "CL", ind = "value", wide = TRUE
      )

      res <- list(ci_dt = ci,pi_dt = pi)
      nn <- sum(grepl("CL", names(ci)))
      if (nn==3){
        out <- merge(ci, pi, by = c(idv, "percentile"))
        nn <- grep("CL", names(out), value = TRUE)[c(1, 3)]
        out[, out_ := value < get(nn[[1]]) | value > get(nn[[2]])]
        out[, zmax := pmax(get(nn[[2]]), value)]
        out[, zmin := pmin(get(nn[[1]]), value)]
        res$out <- out
      }
    } else {
      pi <- quantile_dt(dsim, probs = probs.pi, grp = c(idv, strat), ind = dv)
      pi_area <- dcast(pi[percentile != "p50"],...~percentile)
      res <- list(pi_area_dt = pi_area,pi_dt = pi)
    }
    if (is.null(rug)) {
      rug <- data.frame(x = bins, y = NA_real_, stringsAsFactors = FALSE)
    }
    res$rug_dt <- rug
    res
  }

bin_idv <- function(idv, x) {
  brks <- do.call(classIntervals, append(list(var = idv), x$bin))$brks
  if (max(brks) >= max(idv)) brks[which.max(brks)] <- max(idv)
  if (min(brks) <= min(idv)) brks[which.min(brks)] <- min(idv)
  brks
}



find_interval <- function(x, vec, labels = NULL, ...) {
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
        bins <- x$input[, list(brks = bin_idv(get(idv), x)), c(x$strat.facet)] #bins is a dt, with brks and strat.facet value bin_idv at line 300
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
        x$dx[, bin := find_interval(get(idv), rugs)]
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




vpc.pi_line <- function(dt, left, geom ) {
  mapping <- aes(group = .data$percentile, y = .data$value, linetype=.data$percentile)
  right <- list(data = dt, mapping = mapping)
  left$linetype <- NULL
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
    pp <- ggplot(data = db$pi_dt, aes(x = .data[[if (!is.null(bin)) "bin" else idv]]))

    pi_med_layer <- function() {if (!is.null(pi)) {
      vpc.pi_line(db$pi_dt[percentile == "p50"], pi$median)
    }}
    pi_ext_layer <- function() {if (!is.null(pi) && pi$show == "all") {
      vpc.pi_line(db$pi_dt[percentile != "p50"], pi$extreme)
    }}
    pi_shaded_layer <- function() {if (!is.null(pi) && pi$show %in% c("all","area") ) {
      nn <- grep("^p\\d+$", names(db$pi_area_dt), value = TRUE)

      params <- append(
        list(
          data = db$pi_area_dt,
          mapping = aes(ymin = .data[[nn[[1]]]], ymax = .data[[nn[[2]]]])
        ),
        pi$area
      )
      do.call(geom_ribbon, params)
    }}


    obs_layer <- if (!is.null(obs)) {
      params <- append(
        list(
          mapping = aes(y = .data[[dv]], x = .data[[idv]]),
          data = input
        ),
        obs
      )
      do.call(geom_point, params)
    }

    rug_layer <- if ((!is.null(rug))){
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

    ci_med_layer <- function() {if (!is.null(ci)) {
      nn <- grep("CL", names(db$ci_dt), value = TRUE)[c(1, 3)]
      params <- append(
        list(
          data = db$ci_dt[percentile == "p50"],
          mapping = aes(ymin = .data[[nn[[1]]]], ymax = .data[[nn[[2]]]],
                        group = .data$percentile,fill=.data$percentile)
        ),
        ci$median
      )
      params$fill <- NULL
      do.call(geom_ribbon, params)
    }}
    ci_ext_layer <- function() {if (!is.null(ci) && ci$show == "all") {
      nn <- grep("CL", names(db$ci_dt), value = TRUE)[c(1, 3)]
      params <- append(
        list(
          data = db$ci_dt[percentile != "p50"],
          mapping = aes(ymin = .data[[nn[[1]]]], ymax = .data[[nn[[2]]]],
                        group = .data$percentile,fill=.data$percentile)
        ),
        ci$extreme
      )
      params$fill <- NULL
      do.call(geom_ribbon, params)
    }}

    pp <- ggplot(data = db$pi_dt, aes(x = .data[[if (!is.null(bin)) "bin" else idv]])) +
      obs_layer + pi_med_layer() + pi_ext_layer() + rug_layer

    pp <- if (type=="scatter") pp +  pi_shaded_layer()
    else pp + ci_med_layer() + ci_ext_layer()
    if(!is.null(x$obs_legend)){
      pp <- pp + do.call("scale_linetype_manual",obs_legend)
    }
    if(!is.null(x$sim_legend) && type=="percentile"){
      pp <- pp + do.call("scale_fill_manual",sim_legend)
    }

    strat.facet <- x[["strat.facet"]]

    if (!is.null(strat.facet)) {
      if (is.character(strat.facet)) {
        strat.facet <- formula(paste0("~", strat.facet))
      }
      pp <- pp + do.call("facet_wrap", c(strat.facet, facets))
    }

    if (is.footnote){
      pp <- pp + labs(caption=x$footnote)
    }

    pp
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
#' @param is.footnote \code{logical} if TRUE add footnote
#' @param dname added for compatibility with other ggPMX plots
#' @param facets is a list of parameters passed to facet_wrap in case of startification
#' @param ...  extra parameters passed to base graphical parameters
#'
#' @family vpc
#' @export
#'

pmx_vpc <- function(
  type = c("percentile", "scatter"),
  idv = "TIME",
  obs = pmx_vpc_obs(),
  pi = pmx_vpc_pi(),
  ci = pmx_vpc_ci(),
  rug = pmx_vpc_rug(),
  bin = pmx_vpc_bin(),
  labels = NULL,
  facets = NULL,
  is.legend = TRUE,
  is.footnote= TRUE,
  dname = NULL,
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
      is.footnote = is.footnote,
      type = type,
      facets = facets,
      obs = obs, pi = pi, ci = ci, rug = rug, bin = bin,
      gp = pmx_gpar(labels = labels, is.legend = is.legend, ...)
    ),
    class = c("pmx_vpc", "pmx_gpar")
  )
}


vpc_footnote. <- function(x){

  area_statement <- if(x$type=="percentile"){
    perc <- diff(x$ci$probs)*100
    extension <- if(x$ci$show=="all") "s" else ""
    s <- if(x$ci$show=="all") "" else "s"

    sprintf("The area%s represent%s the %s%% confidence intervals for the percentile%s. ",extension,s,perc,extension)
  } else{
    perc <- diff(x$pi$probs)*100
    if(x$pi$show%in% c("all","area")) sprintf("The area represents the %s%% prediction interval.",perc)
  }
  obs_statement <- if(!is.null(x$obs)) "The dots are the observations."
  rug_statement <- if(!is.null(x$rug)) "The rugs represent the limits of the bins."
  footnote <- paste(area_statement, paste(obs_statement,rug_statement,collapse=" "),
                    "The percentiles are plotted at the median independent variables in the bins.",sep="\n")
  x$footnote <- footnote
  x
}

vpc_legend. <- function(x){
  x$obs_legend <- NULL
  x$sim_legend <- NULL
  percentile <- NULL

  if (!is.null(x$pi)) {
    obs_legend <- list(breaks="p50",values=x$pi$median$linetype,labels="Median")
    if (x$pi$show == "all") {
      breaks <- sort(unique(x$db$pi_dt[,percentile]),decreasing = TRUE)
      labels <- sprintf("%sth percentile",gsub("p0?","",breaks))
      labels <- sub("50th percentile","Median",labels)
      extr_lty <- x$pi$extreme$linetype
      if(length(extr_lty)==1) extr_lty <- rep(extr_lty,2)
      values <- c(extr_lty[2],x$pi$median$linetype,extr_lty[1])
      obs_legend <- list(breaks=breaks,values=values, labels=labels)
    }
    leg_title <- if (x$type =="scatter"){
      if(x$pi$show%in% c("all","area"))
        sprintf("Simulations\n(%s%% Prediction Interval)",diff(x$pi$probs)*100)
      else "Simulations"
    }else  "Observations"

    x$obs_legend <- c(leg_title,obs_legend)
  }

  if (!is.null(x$ci) && x$type=="percentile") {
    sim_legend <- list(breaks="p50",values=x$ci$median$fill,labels="Median")
    if (x$ci$show == "all") {
      breaks <- sort(unique(x$db$ci_dt[,percentile]),decreasing = TRUE)
      labels <- sprintf("%sth percentile",gsub("p0?","",breaks))
      labels <- sub("50th percentile","Median",labels)
      extr_lty <- x$ci$extreme$fill
      if(length(extr_lty)==1) extr_lty <- rep(extr_lty,2)
      values <- c(extr_lty[2],x$ci$median$fill,extr_lty[1])
      sim_legend <- list(breaks=breaks,values=values, labels=labels)
    }
    leg_title <- sprintf("Simulations\n(%s%% CI)",diff(x$ci$probs)*100)
    x$sim_legend <- c(leg_title,sim_legend)
  }

  x$gp$is.legend <- x$is.legend
  default_config <- yaml.load_file(system.file(package = "ggPMX", "init","standing.ppmx"))

  #Check if title/subtitle have default value - if not, custom labels will be set
  if ((x$gp$labels$title == default_config$PMX_VPC$labels$title)
      && (x$gp$labels$subtitle == default_config$PMX_VPC$labels$subtitle)){
    if (x$type == "percentile") {
      x$gp$labels$title <- "Percentile VPC"
      x$gp$labels$subtitle <- "(with observations)"
    } else {
      x$gp$labels$title <- "Scatter VPC"
      x$gp$labels$subtitle <- ""
    }
  }
  if(x$gp$is.title == FALSE) {
    x$gp$labels$title <- ""
    x$gp$labels$subtitle <- ""
  }
  x
}

#' @rdname plot_pmx
#' @export
plot_pmx.pmx_vpc <- function(x, dx, ...) {
  x <- x %>%
    vpc_legend. %>%
    vpc_footnote.

  if (!is.null(x$db)) p <- vpc.plot(x)
  plot_pmx(x$gp, p)
}

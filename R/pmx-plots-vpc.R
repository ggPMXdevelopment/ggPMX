
# note: there is currently only one supported plot of ptype="VPC", namely pname="pmx_vpc"; 
# for simplicity, code associated with the general type and the specific plot are merged
# in a single file

# [temp fix for global bindings]
utils::globalVariables(c("ID", "PRED", "TIME"))


# class constructors for VPC plot configuration objects -----------------------

#' Creates vpc object
#'
#' @param type \code{character} can be either percentile or scatter
#' @param idv \code{character} individual variable
#' @param obs \code{pmx_vpc_obs} object observation layer \link{pmx_vpc_obs}
#' @param pi \code{pmx_vpc_pi} object percentile layer \link{pmx_vpc_pi}
#' @param ci \code{pmx_vpc_ci} object confidence interval layer \link{pmx_vpc_ci}
#' @param rug \code{pmx_vpc_rug} object rug layer \link{pmx_vpc_rug}
#' @param bin \code{pmx_vpc_bin} object \link{pmx_vpc_bin}
#' @param labels \code{list} define title and axis labels
#' @param is.legend \code{logical} if TRUE add legend
#' @param is.footnote \code{logical} if TRUE add footnote
#' @param dname added for compatibility with other ggPMX plots
#' @param facets is a list of parameters passed to facet_wrap in case of startification
#' @param ...  extra parameters passed to base graphical parameters
#'
#' @family vpc
#' @export
#' @returns list with parameters of the vpc object

pmx_vpc <- function(type = c("percentile", "scatter"),
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
#' 
#' @family vpc
#' @returns list with options for `pmx_vpc_bin` object
#' 
pmx_vpc_bin <-
  function(style, within_strat = TRUE, seed = 42, ...) { 
    # within strat = TRUE as default in order to avoid bugs
  
    # set seed for reproducible binning
    # Danielle: would it make more sense to preserve the seed in the return value, and
    # apply it at the time tidyvpc::binning() is called, rather than set the seed here?
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
#' @return list with options for ggplot2 layer with observations
#'
#' @export
#' @family vpc
#' 
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
#' @param show  \code{character} how lines are displayed:
#' \itemize{
#' \item \strong{show=all} lines will be displayed for each of the 3 percentiles, with a shaded area.
#' \item \strong{show=median} Show only median line.
#' \item \strong{show=area} Show only median line and the shaded area
#' }
#' @param interval \code{numeric} quantiles values default to \code{c(.05, .95)}
#' @param median \code{list} containing: \cr
#' \itemize{
#' \item \strong{color} \code{character}  Color of the median percentile line. Default: "#000000".
#' \item \strong{linewidth} \code{numeric}  Thickness of the median percentile line. Default: 1.
#' \item \strong{alpha} \code{numeric} Transparency of the median percentile line. Default: 0.7.
#' \item \strong{linetype} \code{character} Linetype of the median percentile line. Default: "solid".
#' }
#' @param extreme \code{list} containing: \cr
#' \itemize{
#' \item \strong{color} \code{character} Color of the median percentile line. Default: "#000000".
#' \item \strong{linewidth} \code{numeric} Thickness of the median percentile line. Default: 1.
#' \item \strong{alpha} \code{numeric} Transparency of the median percentile line. Default: 0.7.
#' \item \strong{linetype} \code{character} Linetype of the median percentile line. Default: "solid"
#' }
#' @param area \code{list} containing: \cr
#' \itemize{
#' \item \strong{fill} \code{character}  Color of the shaded area. Default: "blue".
#' \item \strong{alpha} \code{numeric} Transparency of the shaded area. Default: 0.1.
#' }
#'
#' @family vpc
#' @returns list with options for Prediction Interval layer
#' @export
#' 
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
#' 
#' @param show  \code{character} how areas are displayed:
#' \itemize{
#' \item \strong{show="all"} areas will be displayed for each of the 3 percentiles.
#' \item \strong{show="median"} Show only median area.
#' }
#' @param interval \code{numeric} quantiles values default to \code{c(.05,.95)}
#' @param method \code{character} which areas are displayed:
#' \itemize{
#' \item \strong{method="ribbon"} areas are ribbons.
#' \item \strong{method="rectangle"} areas are horizontal rectangles.
#' }
#' @param median \code{list} containing: \cr
#' \itemize{
#' \item \strong{fill}  \code{character} Color of the area representing the CI for the median. Default: "#3388cc".
#' \item \strong{alpha} \code{numeric} Transparency of the area representing the PI for the median. Default=0.3.
#' }
#' @param extreme \code{list} containing: \cr
#' \itemize{
#' \item \strong{fill} \code{character} Color of the area representing the CI for the extreme percentiles. Default: "#3388cc".
#' \item \strong{alpha} \code{numeric} Transparency of the area representing the PI for the extreme percentiles. Default=0.3.
#' }
#'
#' @export
#' @returns list with options for confidence interval layer
#' @family vpc
#' 
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
#' When the vpc confidence interval layer method is rectangles we don't show rug separators.
#'
#' @export
#' @returns list with options for the rug layer
#' @family vpc
#' 
pmx_vpc_rug <-
  function(show = TRUE,
           color = "#000000",
           linewidth = 1,
           alpha = 0.7,
           size) {
    if (!missing(size)){
      lifecycle::deprecate_soft("1.2.9", "pmx_vpc_rug(size)", I("use `linewidth=` instead of `size=`"))
      linewidth <- size
    }
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

# S3 plot_pmx() method for any plot of type VPC -------------------------------

#' @rdname plot_pmx
#' @export
plot_pmx.pmx_vpc <- function(x, dx, ...) {
  x <- x %>%
    .vpc_legend %>%
    .vpc_footnote
  if (!is.null(x$db)) p <- .vpc_plot(x) 
  plot_pmx(x$gp, p)
}

# internal functions called during the plot_pmx.pmx_vpc() pipeline ------------

.vpc_pi_line <- function(dt, left, geom) {
  mapping <- aes(group = .data$percentile, y = .data$value, linetype = .data$percentile)
  right <- list(data = dt, mapping = mapping)
  left$linetype <- NULL
  do.call("geom_line", append(right, left))
}

.vpc_plot <- function(x) {
  with(x, {

    # layer functions
    pi_med_layer <- function() {
      if (!is.null(pi)) {
        .vpc_pi_line(db$pi_dt[percentile == "p50"], pi$median)
      }
    }

    pi_ext_layer <- function() {
      if (!is.null(pi) && pi$show == "all") {
        .vpc_pi_line(db$pi_dt[percentile != "p50"], pi$extreme)
      }
    }

    # the bug that breaks scatterplot vpcs is here
    pi_shaded_layer <- function() {
      browser()
      if (!is.null(pi) && pi$show %in% c("all", "area")) {
        nn <- grep("^p\\d+$", names(db$pi_area_dt), value = TRUE)
        params <- append(
          list(
            data = db$pi_area_dt,
            mapping = aes(ymin = .data[[nn[[1]]]], ymax = .data[[nn[[2]]]])
          ),
          pi$area
        )
        do.call(geom_ribbon, params)
      }
    }

    obs_layer <- function() {
      if (!is.null(obs)) {
        params <- append(
          list(
            mapping = aes(y = .data[[dv]], x = .data[[idv]]),
            data = input
          ),
          obs
        )
        do.call(geom_point, params)
      }
    }

    rug_layer <- function() {
      if ((!is.null(rug))) {
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
    }

    ci_med_layer <- function() {
      if (!is.null(ci)) {
        nn <- grep("CL", names(db$ci_dt), value = TRUE)[c(1, 3)]
        params <- append(
          list(
            data = db$ci_dt[percentile == "p50"],
            mapping = aes(
              ymin = .data[[nn[[1]]]], 
              ymax = .data[[nn[[2]]]],
              group = .data$percentile,
              fill=.data$percentile
            )
          ),
          ci$median
        )
        params$fill <- NULL
        do.call(geom_ribbon, params)
      }
    }

    ci_ext_layer <- function() {
      if (!is.null(ci) && ci$show == "all") {
        nn <- grep("CL", names(db$ci_dt), value = TRUE)[c(1, 3)]
        params <- append(
          list(
            data = db$ci_dt[percentile != "p50"],
            mapping = aes(
              ymin = .data[[nn[[1]]]], 
              ymax = .data[[nn[[2]]]],
              group = .data$percentile,
              fill=.data$percentile
            )
          ),
          ci$extreme
        )
        params$fill <- NULL
        do.call(geom_ribbon, params)
      }
    }

    # plot construction
    pp <- ggplot(
      data = db$pi_dt, 
      mapping = aes(x = .data[[if (!is.null(bin)) "bin" else idv]])
    )

    pp <- pp +
      obs_layer() + 
      pi_med_layer() + 
      pi_ext_layer() + 
      rug_layer()

    if (type=="scatter") { 
      pp <- pp + pi_shaded_layer()
    } else {
      pp <- pp + ci_med_layer() + ci_ext_layer()
    }

    if(!is.null(x$obs_legend)) {
      pp <- pp + do.call("scale_linetype_manual", obs_legend)
    }

    if(!is.null(x$sim_legend) && type=="percentile") {
      pp <- pp + do.call("scale_fill_manual", sim_legend)
    }

    strat.facet <- x[["strat.facet"]]

    if (!is.null(strat.facet)) {
      if (is.character(strat.facet)) {
        strat.facet <- stats::as.formula(paste0('~', paste0(strat.facet, collapse = " + ")))
      }
      pp <- pp + do.call("facet_wrap", c(strat.facet, facets))
    }

    if (is.footnote){
      pp <- pp + labs(caption = x$footnote)
    }

    pp
  })
}

.vpc_footnote <- function(x) {
  area_statement <- 
    if (x$type == "percentile") {
      perc <- diff(x$ci$probs) * 100
      extension <- if (x$ci$show == "all") "s" else ""
      s <- if (x$ci$show == "all") "" else "s"
      sprintf(
        "The area%s represent%s the %s%% confidence intervals for the percentile%s. ", 
        extension, s, perc, extension
      )
    } else {
      perc <- diff(x$pi$probs) * 100
      if (x$pi$show %in% c("all","area")) sprintf("The area represents the %s%% prediction interval.", perc)
    }
  obs_statement <- if (!is.null(x$obs)) "The dots are the observations."
  rug_statement <- if (!is.null(x$rug)) "The rugs represent the limits of the bins."
  footnote <- paste(
    area_statement, 
    paste(obs_statement, rug_statement, collapse=" "),
    "The percentiles are plotted at the median independent variables in the bins.",
    sep = "\n"
  )
  x$footnote <- footnote
  x
}

.vpc_legend <- function(x) {
  x$obs_legend <- NULL
  x$sim_legend <- NULL
  percentile <- NULL

  if (!is.null(x$pi)) {
    obs_legend <- list(
      breaks = "p50",
      values = x$pi$median$linetype,
      labels = "Median"
    )
    if (x$pi$show == "all") {
      breaks <- sort(unique(x$db$pi_dt[, percentile]), decreasing = TRUE)
      labels <- sprintf("%sth percentile", gsub("p0?", "", breaks))
      labels <- sub("50th percentile", "Median", labels)
      extr_lty <- x$pi$extreme$linetype
      if (length(extr_lty)==1) extr_lty <- rep(extr_lty, 2)
      values <- c(extr_lty[2], x$pi$median$linetype, extr_lty[1])
      obs_legend <- list(breaks = breaks,values = values, labels = labels)
    }
    leg_title <- 
      if (x$type =="scatter") {
        if (x$pi$show%in% c("all","area")) {
          sprintf("Simulations\n(%s%% Prediction Interval)", diff(x$pi$probs) * 100)
        } else {
          "Simulations"
        }
      } else {
        "Observations"
      }

    x$obs_legend <- c(leg_title, obs_legend)
  }

  if (!is.null(x$ci) && x$type == "percentile") {
    sim_legend <- list(
      breaks = "p50",
      values = x$ci$median$fill,
      labels = "Median"
    )
    if (x$ci$show == "all") {
      breaks <- sort(unique(x$db$ci_dt[, percentile]), decreasing = TRUE)
      labels <- sprintf("%sth percentile", gsub("p0?", "", breaks))
      labels <- sub("50th percentile", "Median", labels)
      extr_lty <- x$ci$extreme$fill
      if (length(extr_lty)==1) extr_lty <- rep(extr_lty,2)
      values <- c(extr_lty[2], x$ci$median$fill, extr_lty[1])
      sim_legend <- list(breaks = breaks, values = values, labels = labels)
    }
    leg_title <- sprintf("Simulations\n(%s%% CI)", diff(x$ci$probs) * 100)
    x$sim_legend <- c(leg_title, sim_legend)
  }

  x$gp$is.legend <- x$is.legend
  default_config <- yaml.load_file(system.file(package = "ggPMX", "init","standing.ppmx"))

  # check if title/subtitle have default value - if not, custom labels will be set
  if (
    (x$gp$labels$title == default_config$PMX_VPC$labels$title) && 
    (x$gp$labels$subtitle == default_config$PMX_VPC$labels$subtitle)
  ) {
    if (x$type == "percentile") {
      x$gp$labels$title <- "Percentile VPC"
      x$gp$labels$subtitle <- "(with observations)"
    } else {
      x$gp$labels$title <- "Scatter VPC"
      x$gp$labels$subtitle <- ""
    }
  }

  if (x$gp$is.title == FALSE) {
    x$gp$labels$title <- ""
    x$gp$labels$subtitle <- ""
  }

  x
}

# internal functions called during pmx_add_plot() pipeline --------------------

.vpc_x <- function(x, self) {
  if (x$ptype != "VPC") return(x) # skip on non VPC plots

  x$dv <- self$dv
  idv <- self$sim[["idv"]]
      
  vpc_stats <- .calculate_vpc_stats(x)
    
  # put VPC parameters into ggPMX list format (ci_dt, pi_dt, out, rug_dt)
  ci_dt <- data.table(vpc_stats$stats) %>%
    dplyr::rename(
      percentile = 'qname',
      TIME = 'xbin',
      CLLOW = 'lo',
      CLMID = 'md',
      CLHIGH = 'hi'
    ) %>%
    dplyr::mutate(
      bin = TIME,
      percentile = as.character(percentile),
      percentile = gsub("^q", "", percentile),
      percentile = as.numeric(percentile) * 100,
      percentile = paste0("p", percentile)
    ) 
    
  # this is not real prediction interval, just a placeholder
  pi_dt <- data.table(vpc_stats$stats) %>%
    dplyr::rename(
      percentile = 'qname',
      TIME = 'xbin',
      value = 'y'
    )  %>%
    dplyr::mutate(
      bin = TIME,
      percentile = as.character(percentile),
      percentile = gsub("^q", "", percentile),
      percentile = as.numeric(percentile) * 100,
      percentile = paste0("p", percentile)
    ) 
  
  # should there be a pi_area here for scatter?



  #This was previosly in the list, but it's not used anyhow if I'm correct
  # out <- data.table(merge(ci_dt, pi_dt, by = c("TIME", "percentile")))
  # nn <- grep("CL", names(out), value = TRUE)[c(1, 3)]
  # #nn <- c('LOW', 'MID', 'HIGH')[c(1, 3)]
  # out[, out_ := value < get(nn[[1]]) | value > get(nn[[2]])]
  # out[, zmax := pmax(get(nn[[2]]), value)]
  # out[, zmin := pmin(get(nn[[1]]), value)]
  
  rug_dt <- data.frame(x = as.numeric(vpc_stats$stats$xbin), y = 1)
    
  #Alex: I don't think this class reassignment makes sense
  old_class <- class(x)
  x$db <- list(
    ci_dt = ci_dt,
    pi_dt = pi_dt,
    # out = out, 
    rug_dt = rug_dt
  )
  class(x) <- old_class
  #x$bin <- as.numeric(x$bin)
  x

}

#' @param x configuration object of class "pmx_vpc"
#' @noRd
.calculate_vpc_stats <- function(x) {

  # construct observed and simulated data sets for VPC
  observed_data <- x$input %>%
    dplyr::filter(!!sym(x$idv)!=0) %>%
    dplyr::arrange(ID, !!sym(x$idv))
  
  simulated_data <- x$dx %>%
    dplyr::arrange(rep, ID, !!sym(x$idv))
  
  observed_data$PRED <- simulated_data %>% 
    dplyr::group_by(ID, !!sym(x$idv)) %>%
    dplyr::summarise(PRED = mean(!!sym(x$dv))) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(PRED)
  
  # extract/parse arguments needed for tidyvpc
  nbins <- ifelse(is.null(x$bin$n), 10, x$bin$n) # what should be a default values for nbins? 
  style <- ifelse(is.null(x$bin$style), 'kmeans', x$bin$style)
  is_predcorr <- ifelse(is.null(x$predcorr), FALSE, x$predcorr) # default to FALSE to match previous
  pi_level <- x$pi$probs 
  ci_level <- x$ci$probs 
  facets   <- x$strat.facet
  if (is.character(facets)) {
    facets <- stats::as.formula(paste0("~", paste0(facets, collapse = " + ")))
  }
  
  # helper functions
  stratify_if <- function(object, facets) {
    if (is.null(facets)) return(object)
    tidyvpc::stratify(object, formula = facets)
  }
  binning_if <- function(object, is_binned, ...) {
    if (!is_binned) return(object) 
    tidyvpc::binning(object, ...)
  }
  binless_if <- function(object, is_binless, ...) {
    if (!is_binless) return(object) 
    tidyvpc::binless(object, ...)
  }
  predcorrect_if <- function(object, is_predcorr) {
    if (!is_predcorr) return(object)
    tidyvpc::predcorrect(object, pred = PRED)
  }

  # compute VPC statistics 
  vpc_stats <- observed_data %>% 
    tidyvpc::observed( 
      # tidyvpc args not yet implemented: blq, lloq, alq, uloq 
      x = !!sym(x$idv), 
      yobs = !!sym(x$dv),
      pred = PRED
    ) %>%
    tidyvpc::simulated(
      simulated_data,
      xsim = !!sym(x$idv), 
      ysim = !!sym(x$dv)
    ) %>%
    stratify_if(facets) %>%
    binning_if(
      # tidyvpc args not yet implemented: 
      # - "breaks" for manual binning
      # - "centers" for manual binning
      # - "altx" but not sure if we want to? 
      is_binned = style != "binless",
      bin = style,     # style arg from ggPMX becomes bins arg to tidybpc
      nbins = nbins,   # this should come from the user
      xbin = "xmedian" # tidyvpc default
    ) %>%
    binless_if(
      is_binless = style == "binless"
    ) %>%
    predcorrect_if(is_predcorr) %>% # does not handle binless predcorr
    tidyvpc::vpcstats(
      # not yet implemented: 
      # - "quantile.type"
      qpred = c(pi_level[1], 0.5, pi_level[2]),
      vpc.type = "continuous", # do we support categorical?
      conf.level = abs(diff(ci_level))
    )
    
  return(vpc_stats)
}


# user-facing function for a specific plot with name "pmx_vpc" ----------------

#' VPC plot
#'
#' @details
#' You can use \link{pmx_vpc_bin} to set the bin parameters. In case of stratification,
#' binning can be different for each strat level (case \code{within_strat} equal to FALSE).
#'
#' @param ctr pmx controller
#' @param ... others graphics parameters passed :
#' \itemize{
#' \item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
#' \item \code{\link{pmx_vpc}} pmx vpc object.
#' \item \code{\link{pmx_update}} function.
#' }
#'
#' \strong{pmx_vpc parameters}
#' 
#' @param type \code{character} can be either percentile or scatter
#' @param idv \code{character} individual variable
#' @param obs \code{pmx_vpc_obs} object observation layer \link{pmx_vpc_obs}
#' @param pi \code{pmx_vpc_pi} object percentile layer  \link{pmx_vpc_pi}
#' @param ci \code{pmx_vpc_ci} object confidence interval layer  \link{pmx_vpc_ci}
#' @param rug  \code{pmx_vpc_rug} object rug layer  \link{pmx_vpc_rug}.
#' Note: consider not using a rug layer when bin[["within_strat"]]=TRUE,
#' since the rugs plotted will not reflect the bins.
#' @param bin \code{pmx_vpc_bin} object  \link{pmx_vpc_bin} specify within pmx_plot_vpc() e.g.: bin = pmx_vpc_bin(style = "kmeans", n = 10)
#' @param predcorr \code{logical} if TRUE apply prediction-correction
#' 
#' @param is.legend \code{logical} if TRUE add legend
#' @param is.footnote \code{logical} if TRUE add footnote
#' @param dname added for compatibility with other ggPMX plots
#'
#' \strong{pmx_update parameters}
#' 
#' @param filter \code{expression} filter which will be applied to plotting data.
#' @param strat.facet \code{formula} optional stratification parameter by facetting.
#' This split plot by strats(each strat in a facet)
#' @param facets \code{list} facet_wrap parameters.
#' @param strat.color \code{character} optional stratification parameter by grouping.
#' This will split the plot by group (color) of strat.
#' @param trans \code{character} define the transformation to apply on x or y or both variables
#' @param pmxgpar a object of class pmx_gpar possibly the output of the
#'
#' \strong{pmx_gpar: Shared basic graphics parameters}
#' 
#' @param labels \code{list} list containing plot and/or axis labels: title, subtitle, x , y
#' @param axis.title \code{list} containing element_text attributes to customize
#' the axis title. (similar to ggplot2 axis.title theme)
#' @param axis.text \code{list} containing element_text attributes to customize
#' the axis text (similar to ggplot2 axis.text theme)
#' @param ranges \code{list} limits of x/y ranges
#' @param is.smooth \code{logical} if set to TRUE add smooth layer
#' @param smooth \code{list} geom_smooth graphical/smoothing fun parameters
#' @param is.band \code{logical} if TRUE add horizontal band
#' @param band \code{list} horizontal band parameters. geom_hline graphical parameters.
#' @param is.draft \code{logical} if TRUE add draft layer
#' @param draft \code{list} draft layer parameters. geom_text graphical parameters.
#' @param is.identity_line \code{logical} if TRUE add an identity line
#' @param identity_line \code{list}geom_abline graphical parameters.
#' @param scale_x_log10 \code{logical} if TRUE use log10 scale for x axis.
#' @param scale_y_log10 \code{logical} if TRUE use log10 scale for y axis.
#' @param color.scales \code{list} define scales parameter in case of strat.color \code{\link{pmx_settings}}
#' @param sim_blq \code{logical} if TRUE uses sim_blq values for plotting. Only for Monolix 2018 and later.
#' @return ggplot2 or list of ggplot2 objects
#' @export
#' @family vpc
#' 
#' @example inst/examples/vpc.R
#'
pmx_plot_vpc <-
  function(
    ctr,  # controller object
    type, # plot type
    idv,  # name of the id variable
    obs,  # layer parameters via pmx_vpc_obs()
    pi,   # layer parameters via pmx_vpc_pi()
    ci,   # layer parameters via pmx_vpc_ci()
    rug,  # layer parameters via pmx_vpc_ci()
    bin,  # layer parameters via pmx_vpc_bin()
    predcorr,    # apply prediction-correction?

    # minor pmx_vpc parameters
    is.legend,   # add legend?
    is.footnote, # add footnote?
    dname,       # for compatibility, not used

    # pmx_update parameters
    filter,
    strat.facet, 
    facets, 
    strat.color, 
    trans, 
    pmxgpar, 

    # shared parameters handled by pmx_gpar
    labels,
    axis.title, 
    axis.text, 
    ranges, 
    is.smooth, 
    smooth, 
    is.band,
    band, 
    is.draft, 
    draft, 
    is.identity_line, 
    identity_line,
    scale_x_log10, 
    scale_y_log10, 
    color.scales, 
    sim_blq, # Danielle: is this really pmx_gpar?
    ...
  ) {
    
    has_rug <- !missing (rug) && !is.null(rug)
    has_bin <- !missing(bin) && !is.null(bin)
    if(has_bin && has_rug) {
      if (isTRUE(bin[["within_strat"]])) {
        warning(
          paste0(
            "Consider not using a rugs layer ",
            "when bin argument has within_strats=TRUE, since the rugs will ",
            "not reflect the bins.",
            "This can be achieved by setting rugs=NULL, or omitting it, ",
            "in the call to pmx_plot_vpc.")
        )
      }
    }

    params <- as.list(match.call(expand.dots = TRUE))[-1]
    params$is.smooth <- FALSE

    # plot-specific params (eventually) end up passed to the S3 method pmx_plot.pmx_vpc(),
    # note also that .vpc_x() is called prior to arrival in pmx_plot()
    wrap_pmx_plot_generic(ctr, "pmx_vpc", params) 
  }








# OLD internal functions called during pmx_add_plot() pipeline --------------------

# .vpc_x_old <- function(x, self) {
#   if (x$ptype == "VPC") {
#     message("calling .vpc_x")
#     x$dv <- self$dv
#     idv <- self$sim[["idv"]]
#     rug <- bin <- brks <- NULL
#     if (!is.null(x$bin)) {
#       if (!is.null(x$strat.facet) && !is.null(x$bin$within_strat) && x$bin$within_strat) {
#         x$bin$within_strat <- NULL
#         bins <- x$input[, list(brks = bin_idv(get(idv), x)), by = c(x$strat.facet)]
        
#         x$input[, bin := {
#           grp <- mget(x$strat.facet)
#           find_interval(get(idv), bins[do.call(paste, c(grp, sep = "_")) == do.call(paste, c(mget(x$strat.facet), sep = "_")), brks])
#         }, by = c(x$strat.facet)]
        
#         x$dx[, bin := {
#           grp <- mget(x$strat.facet)
#           find_interval(get(idv), bins[do.call(paste, c(grp, sep = "_")) == do.call(paste, c(mget(x$strat.facet), sep = "_")), brks])
#         }, by = c(x$strat.facet)]
        
#       } else {
#         rugs <- x$input[, bin_idv(get(idv), x)]
#         x$input[, bin := find_interval(get(idv), rugs)]
#         x$dx[, bin := find_interval(get(idv), rugs)]
#         rug <- data.frame(x = rugs, y = NA_real_, stringsAsFactors = FALSE)
#       }
#     }

#     res <- vpc.data(
#       x[["type"]],
#       x$input,
#       x$dx,
#       x$pi$probs,
#       x$ci$probs,
#       idv = if (!is.null(x$bin)) "bin" else self$sim[["idv"]],
#       irun = self$sim[["irun"]],
#       dv = self$dv,
#       strat = x$strat.facet,
#       rug = rug
#     )
#     old_class <- class(x)
#     x$db <- res
#     class(x) <- old_class
#     x
#   } else {
#     x
#   }
# }

# quantile_dt <-
#   function(dx, grp = "time", ind = "y", probs = c(.05, .95), prefix = "p", wide = FALSE) {
#     percentile <- NULL
#     probs <- sort(unique(c(0.5, probs)))
#     fmt <- ifelse(probs < .1, paste0(prefix, "0%1.f"), paste0(prefix, "%1.f"))
#     probs.n <- sprintf(fmt, probs * 100)
#     if (wide) {
#       dd <- dx[, as.list(stats::quantile(get(ind), probs = probs, na.rm = TRUE)), grp]
#       setnames(dd, grep("%", names(dd)), probs.n)
#     } else {
#       ds <- dx[, stats::quantile(get(ind), probs = probs, na.rm = TRUE), grp]
#       ds[, percentile := rep(probs.n, .N / length(probs))]
#       setnames(ds, "V1", "value")
#     }
#   }

# vpc.data <-
#   function(type = c("percentile", "scatter"),
#            dobs,
#            dsim,
#            probs.pi,
#            probs.ci,
#            idv = "time",
#            irun = "stu",
#            dv = "y",
#            strat = NULL,
#            rug = NULL) {
#     zmax <- zmin <- out_ <- value <- percentile <- NULL
#     bins <- unlist(unique(dobs[, idv, with = FALSE]))
#     if (type == "percentile") {

#       #allow for input e.g. pmx_plot_vpc(strat.facet = ~SEX)
#       if(!is.character(strat)){
#         strat <- all.vars(strat)
#       }

#       pi <- quantile_dt(dobs, probs = probs.pi, grp = c(idv, strat), ind = dv)
#       res2 <- quantile_dt(dsim, probs = probs.pi, grp = c(irun, idv, strat), ind = dv)
#       ci <- quantile_dt(
#         res2,
#         probs = probs.ci, grp = c("percentile", idv, strat),
#         prefix = "CL", ind = "value", wide = TRUE
#       )

#       res <- list(ci_dt = ci,pi_dt = pi)
#       nn <- sum(grepl("CL", names(ci)))
#       if (nn==3){
#         #ALEX: this part of the code was causing errors in VPC when stratyfying by multiple variables.
#         # `out` is not used anywhere in the code as far as I can see, so I commented it out
#         #I keep it like this since we probably upgrade the vpc functionality anyway later
#         #out <- merge(ci, pi, by = c(idv, "percentile"))
#         nn <- grep("CL", names(ci), value = TRUE)[c(1, 3)]
#         #out[, out_ := value < get(nn[[1]]) | value > get(nn[[2]])]
#         #out[, zmax := pmax(get(nn[[2]]), value)]
#         #out[, zmin := pmin(get(nn[[1]]), value)]
#         #res$out <- out
#         res$out <- "DUMMY"
#       }
#     } else {
#       pi <- quantile_dt(dsim, probs = probs.pi, grp = c(idv, strat), ind = dv)
#       pi_area <- dcast(pi[percentile != "p50"],...~percentile)
#       res <- list(pi_area_dt = pi_area,pi_dt = pi)
#     }
#     if (is.null(rug)) {
#       rug <- data.frame(x = bins, y = NA_real_, stringsAsFactors = FALSE)
#     }
#     res$rug_dt <- rug
#     res
#   }

# bin_idv <- function(idv, x) {
#   brks <- do.call(classIntervals, append(list(var = idv), x$bin))$brks
#   if (max(brks) >= max(idv)) brks[which.max(brks)] <- max(idv)
#   if (min(brks) <= min(idv)) brks[which.min(brks)] <- min(idv)
#   brks
# }

# find_interval <- function(x, vec, labels = NULL, ...) {
#   levels <- seq_along(vec)
#   vals <- findInterval(x, vec, rightmost.closed = TRUE, ...)
#   if (!is.null(labels)) {
#     as.numeric(as.character(factor(vals, levels = unique(vals), labels = labels)))
#   } else {
#     stats::ave(x, vals, FUN = stats::median)
#   }
# }

# .vpc.area <- function() {

#   # out <- list(color="red")
#   # out_layer <- if(!is.null(out)){
#   #   params <- append(
#   #     list(
#   #       mapping = aes_string(group="percentile",y="value"),
#   #       data=db$out[(out_)]),
#   #     out)
#   #   do.call(geom_point,params)
#   # }
#   # out_area <- list(fill="red",alpha=0.2)
#   # out_layer_area_min <- if(!is.null(out_area)){
#   #   ll <- list(
#   #     mapping = aes_string(group="percentile",ymin="zmin",ymax=nn[[1]]),
#   #     data=db$out
#   #   )
#   #   params <- append(ll,out_area)
#   #   do.call(geom_ribbon,params)
#   # }
#   #
#   # out_layer_area_max <- if(!is.null(out_area)){
#   #   ll1 <- list(
#   #     mapping = aes_string(group="percentile",ymax="zmax",ymin=nn[[2]]),
#   #     data=db$out
#   #   )
#   #   params <- append(ll1,out_area)
#   #   do.call(geom_ribbon,params)
#   # }

#   # list( out_layer , out_layer_area_min , out_layer_area_max )
# }


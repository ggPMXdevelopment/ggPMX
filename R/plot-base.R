#' @export
plot_pmx.pmx_gpar <- function(x, dx, ...) {
  extra <- list(...)
  #if (length(extra != 1)) stop("plot_pmx for pmx_gpar needs exactly 3 arguments", call.=FALSE)
  if (length(extra) > 1L) {
    plot_pmx_gpar_real(gpar=x, p=dx, bloq_cens=extra[[1]])
  } else {
     plot_pmx_gpar_real(gpar=x, p=dx, bloq_cens="CENS")
  }
}

#' The ggPMX base plot function
#'
#'
#' This function should be called internally by other plots to set
#' general settings like , smoothing, add band, labelling, theming,...
#' @param gpar object of pmx_gpar type
#' @param p plot
#' @param bloq_cens bloq censored column name
#' @import ggplot2
#' @family plot_pmx
#' @return ggplot2 object
#' @export
plot_pmx_gpar_real <- function(gpar, p, bloq_cens) {
  assert_that(is_pmx_gpar(gpar))
  assert_that(is_ggplot(p))
  with(gpar, {
    assert_that(is_list_or_null(smooth))
    assert_that(is_list_or_null(band))
    assert_that(is_list_or_null(labels))
  })

  ## smoothing
  p <- with(gpar, {
    if (is.smooth) {
      if (exists("smooth_with_bloq") && smooth_with_bloq) {
        smooth$data <- p$data[p[["data"]][[bloq_cens]] == 0, ]
      }
      smooth$na.rm <- TRUE
      p <- p + do.call(geom_smooth, smooth)
    }

    if (is.band) {
      p <- p + do.call(geom_hline, band)
    }

    ## labels:title,axis,subtitle...

    ## limits
    if (!is.null(ranges[["y"]])) {
      p <- p %+% scale_y_continuous(limits = ranges[["y"]])
    }

    if (!is.null(ranges[["x"]]) && !discrete) {
      p <- p %+% scale_x_continuous(limits = ranges[["x"]])
    }

    if(is.null(ranges[["x"]])) {
      # Ensure that origin 0 is included on the X axis
      p <- p %+% expand_limits(x=0)
    }

    ## theming
    if (!inherits(gpar$axis.text, "element_text")) {
      gpar$axis.text <- do.call(element_text, as.list(gpar$axis.text))
    }
    if (!inherits(gpar$axis.title, "element_text")) {
      gpar$axis.title <- do.call(element_text, as.list(gpar$axis.title))
    }
    p <- p + pmx_theme(
      axis.text = gpar$axis.text,
      axis.title = gpar$axis.title
    )

    if (is.legend) p <- p + pmx_theme(legend.position = legend.position)

    ## draft layer
    if (is.draft) {
      if (scale_y_log10 && !scale_x_log10) draft$y <- 0
      p <- p + do.call(add_draft, draft)
    }

    ## draft layer
    if (is.identity_line) {
      if ((scale_x_log10 && !scale_y_log10) ||
         (scale_y_log10 && !scale_x_log10))
        p <- p + geom_smooth(method="lm", se=FALSE)
        else p <- p + do.call(geom_abline, identity_line)
    }

    if (scale_x_log10) {
      if (rlang::is_installed("xgxr")) {
        p <- p %+% xgxr::xgx_scale_x_log10(limits = ranges[["x"]])
      } else {
        p <- p %+% scale_x_log10(limits = ranges[["x"]])
      }
      warning("Applying log to x variable will cause nonpositive values to be dropped.")
    }

    if (scale_y_log10) {
      if (rlang::is_installed("xgxr")) {
        p <- p %+% xgxr::xgx_scale_y_log10(limits = ranges[["y"]])
      } else {
        p <- p %+% scale_y_log10(limits = ranges[["y"]])
      }
      warning("Applying log to y-variable will cause nonpositive values to be dropped.")
    }

    if (exists("color.scales", gpar) && !is.null(color.scales)) {
      p <- p + do.call("scale_colour_manual", color.scales)
      p <- p + do.call("scale_fill_manual", color.scales)
    }

    if(!is.title) {
      labels$title <- ""
      labels$subtitle <- ""
    }

    p <- p + with(labels, ggplot2::labs(
      x = x,
      y = y,
      title = title,
      subtitle = subtitle
    ))
    if ("legend" %in% names(labels)) {
      p <- p + with(labels, labs(fill = legend))
    }

    p
  })

  p
}

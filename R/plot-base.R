

#' The ggPMX base plot function
#'
#'
#' This function should be called internally by other plots to set
#' general settings like , smoothing, add band, labelling, theming,...
#' @param gpar object of pmx_gpar type
#' @param p plot
#' @import ggplot2
#' @family plot_pmx
#' @return ggplot2 object
plot_pmx.pmx_gpar <- function(gpar, p) {
  assert_that(is_pmx_gpar(gpar))
  assert_that(is_ggplot(p))
  with(gpar, {
    assert_that(is_list_or_null(smooth))
    assert_that(is_list_or_null(band))
    assert_that(is_list_or_null(labels))
  })



  ## smoothing
  p <- with(gpar, {
    if (has.smooth) {
      smooth$na.rm <- TRUE
      p <- p + do.call(geom_smooth, smooth)
    }

    if (has.band) {
      p <- p + do.call(geom_hline, band)
    }

    ## labels:title,axis,subtitle...
    p <- p + with(labels, ggplot2::labs(
      x = x,
      y = y,
      title = title,
      subtitle = subtitle
    ))
    if ("legend" %in% names(labels)) {
      p <- p + with(labels, labs(fill = legend))
    }

    ## limits
    if (!is.null(ranges$y)) {
      p <- p + scale_y_continuous(limits = ranges$y)
    }
    if (!is.null(ranges$x) && !discrete) {
      p <- p + scale_x_continuous(limits = ranges$x)
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

    if (log_y) {
      if (is.draft) draft$y <- 0
      p <- p + scale_y_log10()
    }
    if (log_x) {
      p <- p + scale_x_log10()
    }

    ## draft layer
    if (is.draft) {
      p <- p + with(draft, add_draft(label, size, color, x, y))
    }

    ## draft layer
    if (has.identity_line) {
      p <- p + with(
        identity_line,
        geom_abline(intercept = intercept, color = color)
      )
    }

    if (exists("color.scales", gpar) && !is.null(color.scales)) {
      p <- p + do.call("scale_colour_manual", color.scales)
      p <- p + do.call("scale_fill_manual", color.scales)
    }
    p
  })
  p
}

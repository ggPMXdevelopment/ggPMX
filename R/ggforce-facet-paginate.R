#' Determine the number of pages in a paginated facet plot
#'
#' This is a simple helper that returns the number of pages it takes to plot all
#' panels when using \code{\link[ggforce]{facet_wrap_paginate}} . It partially builds the plot so depending
#' on the complexity of your plot it might take some time to calculate...
#'
#' @param plot A ggplot object using either facet_wrap_paginate or
#' facet_grid_paginate
#'
#' @return If the plot uses  using either facet_wrap_paginate or
#' facet_grid_paginate it returns the total number of pages. Otherwise it
#' returns NULL
#' @import ggplot2
#' @importFrom ggforce facet_wrap_paginate
#'
n_pages <- function(plot) {
  page <- ggplot_build(plot)$layout$panel_layout$page
  if (!is.null(page)) {
    max(page)
  } else {
    NULL
  }
}


# prevent R CMD check warning for non-standard evaluations
utils::globalVariables(c("iteration", "phase", "Parameter", "Value"))

#' Read MONOLIX SAEM convergence file
#'
#' @param path string specifying data path folder
#' @param ... extra parameter not used
#'
#' @return data.table object

read_mlx_saem_conv <- function(path, ...) {
  pmx_fread(path) 
}

#' SAEM Convergence Plot
#'
#' This plot displays the sequence of estimates for population parameters
#' computed after each iteration of the SAEM algorithm. The purpose is to check
#' the convergence of the algorithm. In addition, a convergence indicator gives
#' the estimation for -2 x log-likelihood along the iterations.
#'
#' @param ctr \code{pmxClass} controller.
#' @param ... additional parameters (not used).
#' @return A \code{ggplot} object showing SAEM convergence plot.
#' @export

pmx_plot_saem_convergence <- function(ctr, ...) {
  params <- get_params_from_call()
  wrap_pmx_plot_generic(ctr = ctr, pname = "saem_convergence", params = params)
}

pmx_saem <- function(labels,
                     facets = NULL,
                     dname = NULL,
                     is.smooth = FALSE,
                     is.reference_line = TRUE,
                     reference_line = NULL,
                     ...) {
  structure(
    list(
      ptype = "SAEM",
      strat = FALSE,
      dname = dname,
      facets = facets,
      is.reference_line = is.reference_line,
      reference_line = reference_line,
      gp = pmx_gpar(labels = labels, is.smooth = is.smooth, ...)
    ),
    class = c("pmx_saem", "pmx_gpar")
  )
}

#' S3 method for plots of class pmx_saem
#'  
#' @param x pmx_saem object
#' @param dx data set
#' @param ... not used
#' @return ggplot2 plot
#' @family plot_pmx
#' @export
#'

plot_pmx.pmx_saem <- function(x, dx, ...) {

  # set column order dynamically
  cols <- names(dx)
  ordered_columns <- setdiff(cols, c("iteration", "phase", "convergenceIndicator"))
  if ("convergenceIndicator" %in% cols) {
    ordered_columns <- c(ordered_columns, "convergenceIndicator")
  }
  
  # pivot to long format
  data_long <- dx %>%
    tidyr::pivot_longer(
      cols = -c(iteration, phase), 
      names_to = "Parameter", 
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Parameter = factor(Parameter, levels = ordered_columns)
    ) 
  
  # base plot
  p <- ggplot(
    data = data_long, 
    mapping = aes(x = iteration, y = Value, group = Parameter)
  ) +
    geom_line(
      mapping = aes(
        color = ifelse(Parameter == "convergenceIndicator", "violet", "blue")
      )
    ) +
    scale_color_identity() + # Use raw colors directly
    facet_wrap(~Parameter, scales = x$facets$scales, ncol = x$facets$ncol)
  
  # add phase 2 reference line if requested
  if (x$is.reference_line) {
    
    first_iteration_phase_2 <- data_long %>%
      dplyr::filter(phase == 2) %>%
      dplyr::slice(1) %>%
      dplyr::pull(iteration)

    p <- p + geom_vline(
      xintercept = first_iteration_phase_2, 
      color = x$reference_line$colour, 
      linetype = x$reference_line$linetype
    ) 
    
  }
  
  p <- plot_pmx(x$gp, p) 
  
  return(p)
}

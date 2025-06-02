
# prevent R CMD check warning for non-standard evaluations
utils::globalVariables(c("iteration", "phase", "parameter", "value"))

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

pmx_param_history <- function(labels,
                              facets = NULL,
                              dname = NULL,
                              is.smooth = FALSE,
                              is.reference_line = TRUE,
                              reference_line = NULL,
                              parameter_line = NULL,
                              convergence_line = NULL,
                              ...) {
  structure(
    list(
      ptype = "PARAM_HISTORY",
      strat = FALSE,
      dname = dname,
      facets = facets,
      is.reference_line = is.reference_line,
      reference_line = reference_line,
      parameter_line = parameter_line,
      convergence_line = convergence_line,
      gp = pmx_gpar(labels = labels, is.smooth = is.smooth, ...)
    ),
    class = c("pmx_param_history", "pmx_gpar")
  )
}

#' S3 method for plots of class pmx_param_history
#'  
#' @param x pmx_param_history object
#' @param dx data set
#' @param ... not used
#' @return ggplot2 plot
#' @family plot_pmx
#' @export
#'

plot_pmx.pmx_param_history <- function(x, dx, ...) {

  # set column order dynamically
  cols <- names(dx)
  ordered_columns <- setdiff(cols, c("iteration", "phase", "convergenceIndicator"))
  if ("convergenceIndicator" %in% cols) {
    ordered_columns <- c(ordered_columns, "convergenceIndicator")
  }
  
  # pivot to long format
  data_long <- dx %>%
    tidyr::pivot_longer(cols = -c(iteration, phase), names_to = "parameter", values_to = "value") %>%
    dplyr::mutate(parameter = factor(parameter, levels = ordered_columns)) 
  
  # base plot
  p <- ggplot(
    data = data_long, 
    mapping = aes(x = iteration, y = value, group = parameter)
  ) +
    geom_line(
      mapping = aes(
        color = ifelse(
          test = parameter == "convergenceIndicator", 
          yes = x$convergence_line$colour, 
          no = x$parameter_line$colour
        ),
        linetype = ifelse(
          test = parameter == "convergenceIndicator", 
          yes = x$convergence_line$linetype, 
          no = x$parameter_line$linetype
        )
      )
    ) +
    scale_color_identity() +
    scale_linetype_identity() +
    facet_wrap(~parameter, scales = x$facets$scales, ncol = x$facets$ncol)
  
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
  
  # add other styling
  p <- plot_pmx(x$gp, p) 
  
  return(p)
}

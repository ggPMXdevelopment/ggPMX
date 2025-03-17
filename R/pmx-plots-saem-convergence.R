#' Read MONOLIX SAEM convergence file
#'
#' @param path string specifying data path folder
#' @param ... extra parameter not used
#'
#' @return data.table object

read_mlx_saem_conv <- function(path, x, ...) {
  saem_file_path <- file.path(path, "ChartsData", "Saem", "CvParam.txt")
  if (!file.exists(saem_file_path)) {
    stop(
      sprintf("The SAEM convergence file can't be found : %s", saem_file_path), 
      call. = FALSE
    )
  }
  dt <- read.table(saem_file_path, header = TRUE, sep = ",")
  dt <- as.data.table(dt) # TODO: this is silly, just read with data table
  return(dt)
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
  wrap_pmx_plot_generic(
    ctr = ctr, 
    pname = "saem_convergence", 
    params = get_params_from_call()
  )
}

# NOTE: this is called in set_plot(), and returns the "conf" object in
# set_plot(), and later becomes the classed "x" object passed to
# plot_pmx.pmx_saem()

pmx_saem <- function(...) {
  
  # TODO: at present the SAEM convergence plot is treated as its own class, so
  # this is a minimal dummy structure, used only to trigger method dispatch:
  # this could be cleaner
  structure(
    list(
      ptype = "SAEM", # plot type
      strat = FALSE,
      dname = "saem"  # data set
    ),
    class = c("pmx_saem", "pmx_gpar")  # class
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

  saem_data <- dx
  
  # set column order dynamically
  cols <- names(saem_data)
  ordered_columns <- setdiff(cols, c("iteration", "phase", "convergenceIndicator"))
  if ("convergenceIndicator" %in% cols) {
    ordered_columns <- c(ordered_columns, "convergenceIndicator")
  }
  
  # pivot to long format for easier visualization
  data_long <- saem_data %>%
    tidyr::pivot_longer(
      cols = -c(iteration, phase), 
      names_to = "Parameter", 
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Parameter = factor(Parameter, levels = ordered_columns)
    ) 
  
  # identify the first iteration of phase 2
  first_iteration_phase_2 <- data_long %>%
    dplyr::filter(phase == 2) %>%
    dplyr::slice(1) %>%
    dplyr::pull(iteration)
  
  # line plot with vertical line denoting phase 2 start, faceted by parameter
  plot <- ggplot(
    data = data_long, 
    mapping = aes(x = iteration, y = Value, group = Parameter)
  ) +
    geom_line(
      mapping = aes(
        color = ifelse(Parameter == "convergenceIndicator", "violet", "blue")
      )
    ) +
    scale_color_identity() + # Use raw colors directly
    geom_vline( # phase 2 indicator
      xintercept = first_iteration_phase_2, 
      color = "red", 
      linetype = "solid"
    ) + 
    facet_wrap(~Parameter, scales = "free_y") +
    labs(
      title = "SAEM Convergence Plot by Parameter", 
      x = "Iteration", 
      y = ""
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10),
      plot.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "none"
    ) 
  
  return(plot)
  
}

#' SAEM Convergence Plot
#'
#' This plot displays the sequence of estimates for population parameters computed after each iteration of the SAEM algorithm. 
#' The purpose is to check the convergence of the algorithm. In addition, a convergence indicator gives the estimation for -2 x log-likelihood along the iterations.
#'
#' @param ctr \code{pmxClass} controller.
#' @param ... additional parameters (not used).
#' @return A \code{ggplot} object showing SAEM convergence plot.
#' @export
#' 
#' 



pmx_plot_saem_convergence <- function(ctr, pname, defaults_, ...) {
  # Determine source directory
  file_dir <- ctr$save_dir
  if (is.null(file_dir)) {
    file_dir <- ctr$private$.data_path
  }
  if (is.null(file_dir)) {
    file_dir <- dirname(ctr$input_file)
  }
  if (is.null(file_dir)) {
    stop("Impossible to determine source directory. Check 'ctr' settings.")
  }
  
  # Build complete file path
  file_path <- file.path(file_dir, "ChartsData/Saem/CvParam.txt")
  if (!file.exists(file_path)) {
    stop(sprintf("The SAEM convergence file can't be found : %s", file_path))
  }
  
  # Read the dataset
  data <- read.table(file_path, header = TRUE, sep = ",")
  
  # Dynamically determine the order of columns (put "convergenceIndicator" last if it exists)
  ordered_columns <- colnames(data)[!colnames(data) %in% c("iteration", "phase", "convergenceIndicator")]
  if ("convergenceIndicator" %in% colnames(data)) {
    ordered_columns <- c(ordered_columns, "convergenceIndicator")
  }
  
  # Transform the dataset into long format for easier visualization and analysis
  data_long <- data %>%
    tidyr::pivot_longer(
      cols = -c(iteration, phase), 
      names_to = "Parameter", 
      values_to = "Value"
    ) %>%
    dplyr::mutate(Parameter = factor(Parameter, levels = ordered_columns))  # Set the factor levels dynamically
  
  # Identify the first iteration of phase 2
  first_iteration_phase_2 <- data_long %>%
    dplyr::filter(phase == 2) %>%
    dplyr::slice(1) %>%
    dplyr::pull(iteration)
  
  # Create a line plot with vertical line indicating phase 2 start and facets for each parameter
  plot <- ggplot(data_long, aes(x = iteration, y = Value, group = Parameter)) +
    geom_line(aes(color = ifelse(Parameter == "convergenceIndicator", "violet", "blue"))) +
    scale_color_identity() + # Use raw colors directly
    geom_vline(xintercept = first_iteration_phase_2, color = "red", linetype = "solid") + # Phase 2 indicator
    facet_wrap(~Parameter, scales = "free_y") +
    labs(
      title = "SAEM Convergence Plot by Parameter", 
      x = "Iteration", 
      y = ""
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10),
      ## strip.background = element_rect( colour = NA),
      plot.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      #plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.position = "none"
    ) 
  
  return(plot)
}

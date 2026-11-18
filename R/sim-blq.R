normalize_sim_blq <- function(data, config) {
  column_names <- names(data)
  config_names <- unique(unlist(
    lapply(
      config[c("sim_blq_npde_iwres", "sim_blq_y", "sim_blq")],
      function(x) names(x$names)
    ),
    use.names = FALSE
  ))

  for (variable in c("iwRes", "pwRes", "npde", "Y", "y")) {
    configured <- grep(paste0("^", variable, "_"), config_names, value = TRUE)
    configured <- intersect(configured, column_names)
    candidates <- paste0(variable, c("_mode_simBlq", "_simBlq", "_simBlq_mode"))
    candidates <- intersect(candidates, column_names)
    source <- if (length(configured) == 1L) configured else head(candidates, 1L)

    if (length(source) == 0L) {
      next
    }
    data[[toupper(variable)]] <- data[[source]]
    # Only a matched simulated response should replace the plotting response.
    if (variable %in% c("Y", "y")) data[["DV"]] <- data[[source]]
  }
  data
}

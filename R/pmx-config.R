function() {
  conf1 <- list(
    data = "standing",
    plots = "standing"
  )

  conf2 <- list(
    data = "standing",
    plots = "inst/templates/mlx/standing/plots.ppmx"
  )

  conf3 <- list(
    data = "inst/templates/mlx/standing/inputs.ipmx",
    plots = "inst/templates/mlx/standing/plots.ppmx"
  )


  conf4 <- list(
    data = "standing",
    plots = "IWRES_IPRED:
  ptype: RES
  x: IPRED
  'y': IWRES
NPDE_TIME:
  ptype: RES
  x: TIME
  'y': NPDE"
  )

  get_config <- function(data_template, plot_template, sys) {
    if (file.exists(data_template)) {
      conf.data <- yaml.load_file(data_template)
    }
    if (file.exists(plot_template)) {
      conf.plots <- yaml.load_file(plot_template)
    }
    config <- list(data = conf.data, plots = conf.plots)
    config$sys <- sys
    class(config) <- "pmxConfig"
    config
  }
}

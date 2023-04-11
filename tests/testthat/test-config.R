context("Test cofig")

#-------------pmx_config START --------------------------------------------
test_that("pmx_config params: inputs result: error", {
  input_dir <-
    file.path(system.file(package = "ggPMX"), "templates", "mlx")
  expect_error(pmx_config(inputs = input_dir))
})

test_that("pmx_config params: plots, inputs result: identical inherits", {
  config <- pmx_config(
    plots = file.path(system.file(package = "ggPMX"), "examples/plots.yaml"),
    inputs = system.file(package = "ggPMX", "examples/custom_inputs.yaml")
  )
  expect_true(inherits(config, "pmxConfig"))
})

test_that("pmx_config params: plots, inputs result: identical names", {
  config <- pmx_config(
    plots = file.path(system.file(package = "ggPMX"), "examples/plots.yaml"),
    inputs = system.file(package = "ggPMX", "examples/custom_inputs.yaml")
  )
  configNames <- c("data", "plots", "sys", "hasNpd")
  expect_identical(names(config), configNames)
})

test_that("pmx_config params: plots result: error inputs template file does not exist", {
  expect_error(pmx_config(
    plots = file.path(system.file(package = "ggPMX"), "examples/plots.yaml")
  ))
})

test_that("pmx_config params: plots result: error plots template file does not exist", {
  expect_error(pmx_config(
    plots = file.path(system.file(package = "ggPMX"), "examples/pl"),
    inputs = system.file(package = "ggPMX", "examples/custom_inputs.yaml")
  ))
})

test_that("pmx_config params: inputs result: default plots is not NULL", {
  config <- pmx_config(
    inputs = system.file(package = "ggPMX", "examples/custom_inputs.yaml")
  )
  expect_false(is.null(config$plots))
})

#-------------pmx_config END ----------------------------------------------

#-------------pmx_get_configs START ----------------------------------------
test_that("can source configs", {
  conf <- pmx_get_configs()
  expect_identical(conf$name, c("standing"))
})

test_that("pmx_get_configs params: NULL result: identical inherits", {
  expect_true(inherits(pmx_get_configs(), c("configs", "data.frame")))
})

test_that("pmx_get_configs params: NULL result: identical names", {
  get_conf <- pmx_get_configs()
  getcNames <- c("sys", "name", "path")
  expect_identical(names(get_conf), getcNames)
})

test_that("pmx_get_configs params: sys = 'nm'; result: NULL", {
  get_conf <- pmx_get_configs(sys = "nm")
  expect_true(is.null(get_conf))
})

test_that("pmx_get_configs params: sys = 'mlx18'; result: identical structure", {
  get_conf <- pmx_get_configs(sys = "mlx18")
  expect_false(is.null(get_conf$path))
  expect_setequal(get_conf$name, c("standing", "standing_old"))
  expect_identical(get_conf$sys, c("mlx18", "mlx18"))
})

test_that("pmx_get_configs params: NULL; result: identical structure", {
  get_conf <- pmx_get_configs()
  expect_false(is.null(get_conf$path))
  expect_identical(get_conf$name, "standing")
  expect_identical(get_conf$sys, "mlx")
})


#-------------pmx_get_configs END ----------------------------------------

#-------------print.configs START ----------------------------------------
test_that("print.configs params: NULL result: error missing arguments", {
  expect_error(print.configs())
})

test_that("print.configs params: NULL result: error class x is not a configs", {
  expect_error(print.configs(x = ""))
})

test_that("print.configs params: x result: message", {
  conf <- pmx_get_configs()
  expect_output(print.configs(conf))
})
#-------------print.configs END ------------------------------------------

#-------------load_config START ------------------------------------------


test_that("load_config params: NULL result: error missing arguments", {
  expect_error(load_config())
})


test_that("load_config params: x, sys result: error x is not a string", {
  sys <- "mlx"
  input_dir <-
    file.path(system.file(package = "ggPMX"), "templates", sys)
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  ifile <- file.path(input_dir, sprintf("%s.i", "standing"))
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", "standing"))
  conf <- load_config_files(ifile, pfile, sys)
  expect_error(load_config(conf$name[1], sys))
})


test_that("can load configs", {
  conf <- pmx_get_configs()
  cfig <- load_config(conf$name[1], "mlx")
  expect_s3_class(cfig, "pmxConfig")
})

test_that("can print loaded config", {
  conf <- pmx_get_configs()
  cfig <- load_config(conf$name[1], "mlx")
  expect_output(print(cfig), "data_name")
  expect_output(print(cfig), "plot_name")
})


test_that("return NULL if bad config name is provided", {
  expect_identical(load_config("BAD_CONFIG_NAME"), NULL)
})
#-------------load_config END ------------------------------------------

#-------------pmx_mlx START ------------------------------------------
theophylline <- file.path(
  system.file(package = "ggPMX"), "testdata",
  "theophylline"
)

WORK_DIR <- file.path(theophylline, "Monolix")
input_file <- file.path(theophylline, "data_pk.csv")

test_that("pmx_mlx params: my_settings with cats.labels, effects and covariates;
          result: identical labels", {
            
  my_settings <- pmx_settings(
    use.labels = TRUE,
    cats.labels = list(SEX = c("0" = "Male", "1" = "Female"), STUD = c("1" = "S", "2" = "D")),
    effects = list(levels = c("ka", "V", "Cl"), labels = c("Absorption_rate", "Volume", "Clearance")),
    covariates = pmx_cov(
      values = list("WT0", "AGE0"),
      labels = list("Weight", "Age")
    )
  )

  ctr <- pmx_mlx(
    config = "standing",
    directory = WORK_DIR,
    input = input_file,
    dv = "Y",
    dvid = "DVID",
    cats = c("SEX"),
    conts = c("WT0", "AGE0"),
    strats = "SEX",
    settings = my_settings,
  )

  p0 <- ctr %>% pmx_plot_eta_hist(strat.facet = ~STUD)
  rlang::quo_text(p0$facet$params$rows) == "structure(list(EFFECT = ~EFFECT), class = c(\"quosures\", \"list\"\n))"

  p <- ctr %>% pmx_plot_eta_box(strat.facet = SEX ~ STUD)
  expect_true(inherits(p$facet$params$rows, "quosures"))

  expect_true(rlang::quo_text(p$facet$params$rows) == "structure(list(SEX = ~SEX), class = c(\"quosures\", \"list\"))")
  expect_true(rlang::quo_text(p$facet$params$cols) == "structure(list(STUD = ~STUD), class = c(\"quosures\", \"list\"))")

  expect_true(all(unique(p$data$EFFECT) %in% c("Absorption_rate", "Volume", "Clearance")))

  # this label came from the my_settings object
  p_effect <- ctr %>% pmx_plot_eta_conts()
  expect_true(all(unique(p_effect$data$variable) %in% c("Weight", "Age")))

  p_cov <- ctr %>% pmx_plot_eta_conts(
    covariates = pmx_cov(
      values = list("WT0", "AGE0"),
      labels = list("Weight", "Age")
    )
  )
  expect_true(all(unique(p_cov$data$variable) %in% c("Weight", "Age")))
})

test_that("pmx_mlx params: my_settings with cats.labels and without effects and covariates;
          result: identical labels", {
  my_settings <- pmx_settings(
    use.labels = TRUE,
    cats.labels = list(SEX = c("0" = "Male", "1" = "Female"), STUD = c("1" = "S", "2" = "D")),
  )

  ctr <- pmx_mlx(
    config = "standing",
    directory = WORK_DIR,
    input = input_file,
    dv = "Y",
    dvid = "DVID",
    cats = c("SEX"),
    conts = c("WT0", "AGE0"),
    strats = "SEX",
    settings = my_settings,
  )

  p <- ctr %>% pmx_plot_eta_box(strat.facet = SEX ~ STUD)
  expect_true(inherits(p$facet$params$rows, "quosures"))

  expect_true(all(unique(p$data$EFFECT) %in% c("ka", "V", "Cl")))

  p_effect <- ctr %>% pmx_plot_eta_conts()
  expect_true(all(unique(p_effect$data$variable) %in% c("WT0", "AGE0")))

  p_cov <- ctr %>% pmx_plot_eta_conts(
    covariates = pmx_cov(
      values = list("WT0", "AGE0"),
      labels = list("Weight", "Age")
    )
  )
  expect_true(all(unique(p_cov$data$variable) %in% c("Weight", "Age")))
})
#-------------pmx_mlx END ---------------------------------------------------

#-------------pmx_warnings START --------------------------------------------
test_that("pmx_warnings: x, warn; result: identical output", {
  ctr <- theophylline()
  pmx_w <- pmx_warnings(ctr, warn = "MISSING_FINEGRID")
  expect_true(is.null(pmx_w))
})


test_that("pmx_warnings: x, warn; result: error x is not a pmxClass", {
  ctr <- ""
  expect_error(pmx_warnings(ctr, "MISSING_FINEGRID"))
})

test_that("pmx_warnings: x, warn; result: error missing arguments", {
  ctr <- theophylline()
  expect_error(pmx_warnings())
})

test_that("pmx_warnings: x, warn; result: message", {
  ctr <- theophylline()
  ctr$warnings <- list(war = "MISSING_FINEGRID")
  expect_message(pmx_warnings(ctr, warn = "war"))
})
#-------------pmx_warnings END ------------------------------------------------

#-------------print.pmxConfig START -------------------------------------------
test_that("print.pmxConfig: x ; result: error x is not a pmxConfig", {
  x <- ""
  expect_error(print.pmxConfig(x))
})

test_that("print.pmxConfig: x ; result: error missing arguments", {
  expect_error(print.pmxConfig())
})

sys <- "mlx"
input_dir <-
  file.path(system.file(package = "ggPMX"), "templates", sys)
plot_dir <- file.path(system.file(package = "ggPMX"), "init")
ifile <- file.path(input_dir, sprintf("%s.ipmx", "standing"))
pfile <- file.path(plot_dir, sprintf("%s.ppmx", "standing"))
config <- load_config_files(ifile, pfile, sys)


test_that("print.pmxConfig : x; result: identical structure", {
  pr <- print.pmxConfig(config)
  expect_identical("mlx", pr$sys)
  expect_true(exists("plots", pr))
  expect_true(exists("data", pr))
})

test_that("print.pmxConfig : x; result: identical names", {
  pr <- print.pmxConfig(config)
  prNames <- c("data", "plots", "sys", "hasNpd")
  expect_identical(prNames, names(pr))

  plotNames <- c(
    "ABS_IWRES_IPRED", "IWRES_IPRED", "IWRES_TIME", "IWRES_DENS",
    "IWRES_QQ", "NPDE_TIME", "NPDE_PRED", "NPDE_QQ",
    "DV_PRED", "DV_IPRED", "INDIVIDUAL", "ETA_HIST",
    "ETA_BOX", "ETA_MATRIX", "ETA_CATS", "ETA_CONTS", "ABS_IWRES_TIME",
    "ETA_QQ", "PMX_VPC"
  )
  print(setdiff(names(pr$plots), plotNames))
  dataNames <- c("predictions", "estimates", "eta", "finegrid")
  expect_setequal(plotNames, names(pr$plots))
  expect_setequal(dataNames, names(pr$data))
})


test_that("print.pmxConfig : x; result: identical inherits", {
  pr <- print.pmxConfig(config)
  expect_true(inherits(pr, "pmxConfig"))
})

#-------------print.pmxConfig END ----------------------------------------------

#-------------load_config_files START ------------------------------------------

test_that("load_config_files : ifile, pfile, sys; result: identical inherits", {
  sys <- "mlx"
  input_dir <-
    file.path(system.file(package = "ggPMX"), "templates", sys)
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  ifile <- file.path(input_dir, sprintf("%s.ipmx", "standing"))
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", "standing"))
  config <- load_config_files(ifile, pfile, sys)
  expect_true(inherits(config, "pmxConfig"))
})

test_that("load_config_files : params: NULL; result: error misssing arguments", {
  expect_error(load_config_files())
})


test_that("load_config_files : ifile, pfile, sys; result:  ifile do not exist", {
  sys <- "mlx"
  input_dir <-
    file.path(system.file(package = "ggPMX"), "templates", sys)
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  ifile <- file.path(input_dir, sprintf("%s.i", "standing"))
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", "standing"))
  config <- load_config_files(ifile, pfile, sys)
  expect_true(is.null(config))
})

test_that("load_config_files : ifile, pfile, sys; result:  pfile do not exist", {
  sys <- "mlx"
  input_dir <-
    file.path(system.file(package = "ggPMX"), "templates", sys)
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  ifile <- file.path(input_dir, sprintf("%s.ipmx", "standing"))
  pfile <- file.path(plot_dir, sprintf("%s.x", "standing"))
  config <- load_config_files(ifile, pfile, sys)
  expect_true(is.null(config))
})
#-------------load_config_files END --------------------------------------------

context("Test pmxClass")
pmxClassHelpers <- test_pmxClass_helpers()

#test_that("can create pmx class", {
#  ctr <- pmxClassHelpers$ctr
#  expect_is(ctr, "pmxClass")
#  expect_identical(
#    sort(ctr %>% plot_names()),
#    sort(c(
#      "abs_iwres_ipred", "abs_iwres_time", "iwres_ipred", "npde_time", "iwres_time",
#      "npde_pred", "dv_pred", "dv_ipred", "eta_hist", "eta_box", "individual",
#      "eta_matrix", "eta_cats", "eta_conts",
#      "iwres_qq", "npde_qq", "eta_qq", "iwres_dens"
#    ))
#  )
#})
#
#test_that("can print pmx class", {
#  ctr <- pmxClassHelpers$ctr
#  expect_output(print(ctr), "pmx object:")
#})
#
#test_that("can remove plot from pnmx class", {
#  ctr <- pmxClassHelpers$ctr
#  cplots <- ctr %>% plot_names()
#  ctr$remove_plot(cplots[1])
#  res <- setdiff(cplots, ctr %>% plot_names())
#  expect_identical(res, cplots[1])
#})
#
#test_that("can get pmx class config", {
#  ctr <- pmxClassHelpers$ctr
#  cplots <- ctr %>% plot_names()
#  conf <- ctr$get_config("npde_time")
#  clabels <- list(
#    title = "NPDE vs TIME",
#    subtitle = "",
#    x = "TIME",
#    y = "NPDE"
#  )
#  expect_identical(conf$gp$labels, clabels)
#})
##------------------- get_data - start ------------------------------------------
#
#test_that("get_data: params NULL result: error missing arguments", {
#  expect_error(get_data())
#})
#
#test_that("get_data: params ctr result: error ctr is not a pmxClass object", {
#  ctr <- ""
#  expect_error(get_data(ctr))
#})
#
#test_that("can get data from controller", {
#  ctr <- pmxClassHelpers$ctr
#  get_d <- ctr %>% get_data(data_set = "individual")
#  expect_true(inherits(get_d, c("data.table", "data.frame")))
#})
#
#test_that("can get data from controller", {
#  ctr <- pmxClassHelpers$ctr
#  inputData <- ctr %>% get_data("input")
#  inNames <- c("ID", "DV", "TIME", "SEX", "WT0", "AGE0", "STUD")
#  expect_true(all(inNames %in% names(inputData)))
#
#  peData <- ctr %>% get_data("estimates")
#  peNames <- c("PARAM", "VALUE", "SE", "RSE", "PVALUE")
#  expect_identical(names(peData), peNames)
#
#  mpData <- ctr %>% get_data("predictions")
#  mpNames <- c(
#    "ID", "TIME", "PRED", "NPDE", "IPRED", "IWRES", "DV",
#    "SEX", "WT0", "AGE0", "STUD"
#  )
#  expect_true(all(mpNames %in% names(mpData)))
#
#  fgData <- ctr %>% get_data("finegrid")
#  fgNames <- c("ID", "TIME", "PRED", "IPRED")
#  expect_true(all(fgNames %in% names(fgData)))
#})
#
##------------------- get_data - end --------------------------------------------
#
##------------------- set_plot - start ------------------------------------------
#
#test_that("set_plot: params NULL result: error missing arguments", {
#  expect_error(set_plot())
#})
#
#
#test_that("set_plot: params ctr result: error ctr is not a pmxClass object", {
#  ctr <- ""
#  expect_error(set_plot(ctr))
#})
#
#test_that("set_plot: params: ptype, pname, type result: error pname is not a character or NULL", {
#  ctr <- pmxClassHelpers$ctr
#  expect_error(ctr %>% set_plot("DIS", pname = 1, type = "box"))
#})
#
#test_that("set_plot: params: ptype, pname result: error strat.color is not a character or NULL", {
#  ctr <- pmxClassHelpers$ctr
#  expect_error(ctr %>% set_plot("DIS", pname = "distr2", strat.color = TRUE))
#})
#
#test_that("set_plot: params: ptype, pname result: error strat.facet is not a character or formula or NULL", {
#  ctr <- pmxClassHelpers$ctr
#  expect_error(ctr %>% set_plot("DIS", pname = "distr2", strat.facet = TRUE))
#})
#
#
#test_that("set_plot: params: ptype, pname result: identical names", {
#  ctr <- pmxClassHelpers$ctr
#  spl <- ctr %>% set_plot("DIS", pname = "distr1", type = "box")
#  setNames <- c(
#    ".__enclos_env__", "sim_blq", "time", "id", "bloq", "sim",
#    "plot_file_name", "report_n", "report_queue", "save_dir", "footnote", "warnings",
#    "endpoint", "abbrev", "re", "has_re", "settings", "strats",
#    "occ", "conts", "cats", "dvid", "dv", "input_file",
#    "input", "config", "data", "clone", "post_load", "plots",
#    "get_plot", "set_config", "get_config", "remove_plot", "update_plot", "add_plot",
#    "dequeue_plot", "enqueue_plot", "print", "initialize"
#  )
#  expect_identical(setNames, names(spl))
#})
#
#test_that("can set plot and filter", {
#  # set new plot
#  ctr <- pmxClassHelpers$ctr
#  ctr %>% set_plot("DIS", pname = "distr1", type = "box")
#  p <- ctr %>% get_plot("distr1")
#  pconf <- ggplot2::ggplot_build(p)
#  expect_equal(length(pconf$data), 5)
#  # set plot and filter
#  ctr %>% set_plot("DIS", pname = "distr2", filter = ID < 10, type = "box")
#  p <- ctr %>% get_plot("distr2")
#  pconf <- ggplot2::ggplot_build(p)
#  expect_equal(length(pconf$data), 5)
#})
#
##------------------- set_plot - end --------------------------------------------
#
#test_that("can disable draft for all plots", {
#  theophylline <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#  WORK_DIR <- file.path(theophylline, "Monolix")
#  input_file <- file.path(theophylline, "data_pk.csv")
#
#  ctr <-
#    pmx_mlx(
#      config = "standing",
#      directory = WORK_DIR,
#      input = input_file,
#      dv = "Y",
#      dvid = "DVID",
#      cats = c("SEX"),
#      conts = c("WT0", "AGE0"),
#      strats = "STUD",
#      settings = pmx_settings(is.draft = FALSE)
#    )
#
#  is_draft <- vapply(
#    ctr %>% plot_names(),
#    function(p) {
#      conf <- ctr %>% get_plot_config(p)
#      conf$gp[["is.draft"]]
#    }, TRUE
#  )
#  expect_false(any(is_draft))
#})
#
#
#test_that("can set draft to false for a single plot", {
#  ctr <- pmxClassHelpers$ctr
#  p <- ctr %>% pmx_plot_dv_pred(is.draft = FALSE)
#})
#
#
#
#test_that("can create a controller with data.frame as input", {
#  theophylline <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#  WORK_DIR <- file.path(theophylline, "Monolix")
#  input_file <- file.path(theophylline, "data_pk.csv")
#
#  dat <- read.csv(input_file)
#  dat$SEX <- factor(dat$SEX, levels = c(0, 1), labels = c("M", "F"))
#
#
#  ctr4 <- pmx(
#    config = "standing", sys = "mlx",
#    directory = WORK_DIR,
#    input = dat,
#    dv = "Y",
#    dvid = "DVID",
#    cats = "SEX"
#  )
#
#  expect_equal(nrow(ctr4 %>% get_data("input")), nrow(dat))
#})
#
##------------------- pmx_settings - start ---------------------------------------
#
#test_that("pmx_settings: params effects result: error effects should be a list", {
#  effects <- c(
#    levels = c("ka", "V", "Cl"),
#    labels = c("Concentration", "Volume", "Clearance")
#  )
#  expect_error(pmx_settings(effects = effects))
#})
#
#test_that("pmx_settings: params effects result: error effects should be a list
#          that contains levels and labels", {
#  effects <- list(
#    lev = c("ka", "V", "Cl"),
#    lab = c("Concentration", "Volume", "Clearance")
#  )
#  expect_error(pmx_settings(effects = effects))
#})
#
#test_that("pmx_settings: params effects result: error effects should be a list
#          that contains levels and labels have the same length", {
#  effects <- list(
#    levels = c("ka"),
#    labels = c("Concentration", "Volume", "Clearance")
#  )
#  expect_error(pmx_settings(effects = effects))
#})
#
#
#test_that("pmx_settings: params is.draft result: identical names", {
#  setg <- pmx_settings(is.draft = FALSE)
#  settNames <- c(
#    "is.draft", "use.abbrev", "color.scales", "use.labels", "cats.labels",
#    "use.titles", "effects", "covariates"
#  )
#  expect_identical(settNames, names(setg))
#})
#
#test_that(
#  "can create controller global settings",
#  expect_is(pmx_settings(), "pmxSettingsClass")
#)
#
##------------------- pmx_settings - start --------------------------------------
#
##------------------- pmx_mlxtran - start ---------------------------------------
#
#test_that("pmx_mlxtran: params NULL result: error missing arguments", {
#  expect_error(pmx_mlxtran())
#})
#
#
#test_that("pmx_mlxtran: params: file_name, endpoint, ect.;  result: identical structure", {
#  mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "*_popPK_model", "project.mlxtran")
#  ep <- pmx_endpoint(
#    code = "1",
#    file.code = "2"
#  )
#  ctr <- pmx_mlxtran(file_name = mlxtran_path, version = 1, endpoint = ep)
#  expect_identical(ctr$endpoint$file.code, "2")
#})
#
#test_that("pmx_mlxtran: params: file_name, endpoint, call = TRUE, ect.;  result: identical sturcture", {
#  mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "*_popPK_model", "project.mlxtran")
#  ep <- pmx_endpoint(
#    code = "1",
#    file.code = "2"
#  )
#  ctr <- pmx_mlxtran(file_name = mlxtran_path, version = 1, endpoint = ep, call = TRUE)
#  expect_identical(ctr$config, "standing")
#  expect_identical(ctr$dvid, "YTYPE")
#  expect_identical(ctr$dv, "DV")
#})
#
#
#test_that("can create a controller from mlxtran with explicit path", {
#  mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "1_popPK_model", "project.mlxtran")
#  ctr <- pmx_mlxtran(file_name = mlxtran_path)
#  expect_is(ctr, "pmxClass")
#})
#
#test_that("can catch absence of version, when wildcard is used in file_name", {
#  mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "*_popPK_model", "project.mlxtran")
#  error_msg_wrong_version <- "Using wildcard in file_name assume providing non-negative version"
#  error_msg_not_exist <- "file do not exist"
#  expect_error(pmx_mlxtran(file_name = mlxtran_path), error_msg_wrong_version, fixed = TRUE)
#  expect_error(pmx_mlxtran(file_name = mlxtran_path, version = -5), error_msg_wrong_version, fixed = TRUE)
#  expect_error(pmx_mlxtran(file_name = mlxtran_path, version = 2), error_msg_not_exist, fixed = TRUE)
#})
#
#test_that("can create a controller from mlxtran with wildcard in path", {
#  mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "*_popPK_model", "project.mlxtran")
#  ctr <- pmx_mlxtran(file_name = mlxtran_path, version = 1)
#  expect_is(ctr, "pmxClass")
#})
##------------------- pmx_mlxtran - end -----------------------------------------
#
##------------------- pmx_sim - start -------------------------------------------
#
#test_that("pmx_sim: params result: pmxSimClass, list", {
#  theo_path <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#  vpc_file <- file.path(theo_path, "sim.csv")
#  sim <- pmx_sim(
#    file = vpc_file,
#    irun = "rep",
#    idv = "TIME"
#  )
#
#  expect_true(inherits(sim, c("pmxSimClass", "list")))
#  expect_identical(sim$irun, "rep")
#  simNames <- c("sim", "irun", "idv")
#  expect_true(all(simNames %in% names(sim)))
#  expect_error(pmx_sim())
#})
#
#test_that("pmx_sim: params result: default values of the arguments", {
#  theo_path <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#  vpc_file <- file.path(theo_path, "sim.csv")
#  sim <- pmx_sim(
#    file = vpc_file,
#    irun = "rep"
#  )
#
#  expect_identical(sim$data, NULL)
#  expect_identical(sim$idv, "TIME")
#  expect_true(inherits(sim$sim, c("data.table", "data.frame")))
#})
##------------------- pmx_sim - end ---------------------------------------------
#
##------------------- check_argument - start ------------------------------------
#
#test_that("check_argument: params value, pmxname = 'work_dir' result: identical inherits", {
#  theophylline <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#  WORK_DIR <- file.path(theophylline, "Monolix")
#  directory <- check_argument(WORK_DIR, "work_dir")
#  expect_true(inherits(directory, "character"))
#})
#test_that("check_argument: params value, pmxname = 'input' result: identical inherits", {
#  theophylline <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#
#  input_file <- file.path(theophylline, "data_pk.csv")
#  input <- check_argument(input_file, "input")
#  expect_true(inherits(input, "character"))
#})
#
#test_that("check_argument: params NULL result: error missing arguments", {
#  expect_error(check_argument())
#})
#
#test_that("check_argument: params value = NULL, pmxname  result: error set a NULL argument", {
#  expect_error(check_argument(value = NULL, pmxname = "work_dir"))
#})
#
##------------------- check_argument - end --------------------------------------
#
##------------------- pmx - start -----------------------------------------------
#test_that("pmx: params NULL result: error missing arguments", {
#  expect_error(pmx())
#})
#
#test_that("pmx: params: fit result: default values of the arguments ", {
#  theophylline <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#  WORK_DIR <- file.path(theophylline, "Monolix")
#  input_file <- file.path(theophylline, "data_pk.csv")
#  pm <- pmx(directory = WORK_DIR, input = input_file, dv = "EVID")
#  settings <- pmx_settings()
#
#  expect_identical(pm$cats, "")
#  expect_identical(pm$conts, "")
#  expect_identical(pm$occ, "")
#  expect_identical(pm$strats, "")
#  expect_false(pm$sim_blq)
#  expect_identical(pm$dvid, "DVID")
#  expect_identical(pm$endpoint, NULL)
#  expect_identical(pm$settings, settings)
#  expect_identical(pm$bloq, NULL)
#  expect_identical(pm$sim, NULL)
#})
#
#test_that("pmx: params; result:  error class cat, conts, occ, strats, bloq
#          are not valid character vectors", {
#  theophylline <- file.path(
#    system.file(package = "ggPMX"), "testdata",
#    "theophylline"
#  )
#  WORK_DIR <- file.path(theophylline, "Monolix")
#  input_file <- file.path(theophylline, "data_pk.csv")
#  expect_error(pmx(directory = WORK_DIR, input = input_file, dv = "EVID", cats = 1))
#  expect_error(pmx(directory = WORK_DIR, input = input_file, dv = "EVID", conts = 1))
#  expect_error(pmx(directory = WORK_DIR, input = input_file, dv = "EVID", occ = 1))
#  expect_error(pmx(directory = WORK_DIR, input = input_file, dv = "EVID", strats = 1))
#  expect_error(pmx(directory = WORK_DIR, input = input_file, dv = "EVID", bloq = 1))
#})
#
##------------------- pmx - end -------------------------------------------------
#
#------------------- pmx_mlx - end ---------------------------------------------

test_that("pmx_mlx: params NULL result: error missing arguments", {
  expect_error(pmx_mlx())
})

test_that("pmx_mlx: params; result: identical inherits", {
  theophylline <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "theophylline"
  )
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")

  ctr <-
    pmx_mlx(
      config = "standing",
      directory = WORK_DIR,
      input = input_file,
      dv = "Y",
      dvid = "DVID",
      cats = c("SEX"),
      conts = c("WT0", "AGE0"),
      strats = "STUD",
      settings = pmx_settings(is.draft = FALSE)
    )
  expect_true(inherits(ctr, c("pmxClass", "R6")))
})
#------------------- pmx_mlx - end ---------------------------------------------

#------------------- formula_to_text - end -------------------------------------

test_that("formula_to_text: params NULL result: error missing arguments", {
  expect_error(formula_to_text())
})

test_that("formula_to_text: params form result: identical inherits", {
  f1 <- formula_to_text("strat.facet")
  f2 <- formula_to_text(EFFECT ~ variable)
  f3 <- formula_to_text(1)
  expect_true(inherits(f1, "character"))
  expect_true(inherits(f2, "character"))
  expect_true(inherits(f3, "numeric"))
})

test_that("pmx_settings are applied to the plot", {
  my_settings <- pmx_settings(
    effects=list(
      levels=c("Cl","ka","V"),
      labels=c("Clearance", "Absorption_rate", "Volume")
    ),
    covariates=pmx_cov(values=list("SEX"), labels=list("Sex"))
  )

  theophylline <- file.path(
    system.file(package="ggPMX"),
    "testdata",
    "theophylline"
  )

  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")

  ctr <- pmx_mlx(
    directory=WORK_DIR,
    input=input_file,
    dv="Y",
    dvid="DVID",
    conts=c("WT0", "AGE0"),
    settings=my_settings,
    cats=c("SEX"),
    config="standing", strats=c("STUD")
  )

  p <- ctr %>% pmx_plot_eta_cats()
  expect_identical(levels(p[["data"]][["variable"]]), c("Sex"))
  expect_identical(
    levels(p[["data"]][["EFFECT"]]),
    c("Clearance", "Absorption_rate", "Volume")
  )
})


#------------------- formula_to_text - end -------------------------------------

#------------------- pmx_endpoint - start --------------------------------------

test_that("pmx_endpoint: params NULL result: error missing arguments", {
  expect_error(pmx_endpoint())
})

test_that("pmx_endpoint: params: code, file.code, ect.; result: error code is not a character vector ", {
  expect_error(pmx_endpoint(
    code = 3,
    file.code = "1"
  ))
})

test_that("pmx_endpoint: params: code, file.code, ect.; result: error file.code is not a character vector ", {
  expect_error(pmx_endpoint(
    code = "3",
    file.code = 1
  ))
})

test_that("pmx_endpoint: params: code, file.code, ect.; result: error label is not a character vector ", {
  expect_error(pmx_endpoint(
    label = 1,
    code = "3",
    file.code = "1"
  ))
})

test_that("pmx_endpoint: params: code, file.code, ect.; result: error trans is not a character vector ", {
  expect_error(pmx_endpoint(
    trans = NA,
    code = "3",
    file.code = "1"
  ))
})

test_that("pmx_endpoint: params: code, file.code, ect.; result: error unit is not a character vector ", {
  expect_error(pmx_endpoint(
    unit = TRUE,
    code = "3",
    file.code = "1"
  ))
})

test_that("pmx_endpoint: params: code, file.code, ect.; result: identical inherits ", {
  ep <- pmx_endpoint(
    code = "3",
    file.code = "1"
  )
  expect_true(inherits(ep, "pmxEndpointClass"))
})

test_that("pmx_endpoint: params: code, file.code, ect.; result: identical names ", {
  ep <- pmx_endpoint(
    code = "3",
    file.code = "1"
  )
  epNames <- c("code", "label", "unit", "file.code", "trans")
  expect_identical(names(ep), epNames)
})

#------------------- pmx_endpoint - end ----------------------------------------

#------------------- pmx_bloq - start ------------------------------------------

test_that("pmx_bloq: params NULL result: identical inherits", {
  expect_true(inherits(pmx_bloq(), "pmxBLOQClass"))
})

test_that("pmx_bloq: params: cens, limit; result: identical names ", {
  bloq <- pmx_bloq(cens = "BLOQ_name", limit = "LIMIT_name")
  blNames <- c("cens", "limit", "show", "colour", "size", "linewidth", "alpha")
  expect_identical(names(bloq), blNames)
})

test_that("pmx_bloq: params: cens, limit; result: identical structure ", {
  bloq <- pmx_bloq(cens = "BLOQ_name", limit = "LIMIT_name")
  expect_true(bloq$show)
  expect_identical(bloq$cens, "BLOQ_name")
  expect_identical(bloq$colour, "pink")
})

#------------------- pmx_bloq - end --------------------------------------------

#------------------- print.abbreviation - start --------------------------------

test_that("print.abbreviation: params x result: x is not an abbreviation ", {
  expect_error(print.abbreviation(x = ""))
})

test_that("print.abbreviation: params NULL result: error missing arguments", {
  expect_error(print.abbreviation())
})

#------------------- print.abbreviation - end ----------------------------------

#------------------- get_abbrev - start ----------------------------------------

test_that("get_abbrev: params NULL result: error missing arguments", {
  expect_error(get_abbrev())
})

#------------------- get_abbrev - end ------------------------------------------

#------------------- set_abbrev - start ----------------------------------------

test_that("set_abbrev: params ctr result: ctr is not a pmxClass", {
  ctr <- ""
  expect_error(ctr %>% set_abbrev())
})

test_that("set_abbrev: params NULL result: error missing arguments", {
  expect_error(set_abbrev())
})

test_that("set_abbrev: params ctr result: error attempt to set an attribute on NULL", {
  ctr <- pmxClassHelpers$ctr
  ctr$abbrev <- NULL
  expect_error(ctr %>% set_abbrev())
})
#------------------- set_abbrev - end ------------------------------------------

#------------------- get_plot - start ------------------------------------------

test_that("get_plot: params NULL result: error missing arguments", {
  expect_error(get_plot())
})

test_that("get_plot: params: ctr, nplot, which_pages  result: identical inherits", {
  ctr <- pmxClassHelpers$ctr
  get_p <- get_plot(ctr, nplot = "individual", which_pages = 1)
  expect_true(inherits(get_p, c("gg", "ggplot")))
})

test_that("get_plot: params: ctr, nplot, which_pages  result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(get_plot(ctr, nplot = "individual", which_pages = 1))
})

test_that("get_plot: params: ctr, nplot, which_pages  result: error nplot is not a character", {
  ctr <- pmxClassHelpers$ctr
  expect_error(get_plot(ctr, nplot = list("individual", "eta_hist"), which_pages = 1))
})

test_that("get_plot: params: ctr, nplot, which_pages  result: error which_pages
          is not an integer or 'all' or 1L", {
  ctr <- pmxClassHelpers$ctr
  expect_error(get_plot(ctr, nplot = "individual", which_pages = "one"))
})

test_that("get_plot: params: ctr, nplot, which_pages  result: error nplot is not valid plot name", {
  ctr <- pmxClassHelpers$ctr
  expect_error(get_plot(ctr, nplot = "indiv", which_pages = 1L))
})

test_that("get_plot: params: ctr, nplot, which_pages  result: identical structure", {
  ctr <- pmxClassHelpers$ctr
  get_p <- get_plot(ctr, nplot = "individual", which_pages = 1L)
  expect_equal(get_p$facet$params$nrow, 4)
})

test_that("get_plot: params: ctr, nplot, which_pages  result: identical names", {
  ctr <- pmxClassHelpers$ctr
  get_p <- get_plot(ctr, nplot = "individual", which_pages = 1L)
  gplNames <- c(
    "data", "layers", "scales", "mapping", "theme", "coordinates",
    "facet", "plot_env", "labels"
  )
  expect_identical(gplNames, names(get_p))
})

#------------------- get_plot - end --------------------------------------------

#------------------- plot_names - start ----------------------------------------

test_that("plot_names: params NULL result: error missing arguments", {
  expect_error(plot_names())
})

test_that("plot_names: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(plot_names(ctr))
})

#------------------- plot_names - end ------------------------------------------

#------------------- plots - start ---------------------------------------------

test_that("plots: params NULL result: error missing arguments", {
  expect_error(plots())
})

test_that("plots: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(plots(ctr))
})

test_that("plots: params: ctr result: identical inherits", {
  ctr <- pmxClassHelpers$ctr
  pp <- plots(ctr)
  expect_true(inherits(pp, c("data.table", "data.frame")))
  expect_true(inherits(pp[[1]], "character"))
})

test_that("plots: params: ctr result: identical names", {
  ctr <- pmxClassHelpers$ctr
  pp <- plots(ctr)
  pNames <- c("plot_name", "plot_type", "plot_function")
  expect_identical(pNames, names(pp))
})

#------------------- plots - end -----------------------------------------------

#------------------- get_plot_config - start -----------------------------------

test_that("get_plot_config: params NULL result: error missing arguments", {
  expect_error(get_plot_config())
})

test_that("get_plot_config: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(get_plot_config(ctr))
})

test_that("get_plot_config: params: ctr, pname result: identical inherits", {
  ctr <- pmxClassHelpers$ctr
  get_pconf <- get_plot_config(ctr, "individual")
  expect_true(inherits(get_pconf, c("individual", "pmx_gpar")))
})

test_that("get_plot_config: params: ctr, pname result: identical structure", {
  ctr <- pmxClassHelpers$ctr
  get_pconf <- get_plot_config(ctr, "individual")
  expect_identical(get_pconf$ptype, "IND")
})

#------------------- get_plot_config - end -------------------------------------

#------------------- set_data - start ------------------------------------------

test_that("set_data: params NULL result: error missing arguments", {
  expect_error(set_data())
})

test_that("set_data: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(set_data(ctr))
})

test_that("set_data: params: ctr result: error each data set should be well named", {
  ctr <- pmxClassHelpers$ctr
  expect_error(set_data(ctr, "eta"))
})

test_that("set_data: params: ctr result: error arguments is of length zero", {
  ctr <- pmxClassHelpers$ctr
  expect_error(set_data(ctr))
})

test_that("set_data: params: ctr, eta result: identical names, inherits", {
  ctr <- theophylline()
  sd <- set_data(ctr, eta = get_data(ctr, "eta")[, EFFECT := factor(
    EFFECT,
    levels = c("ka", "V", "Cl"),
    labels = c("Concentration", "Volume", "Clearance")
  )])
  expect_identical(names(sd), "eta")
  expect_true(inherits(sd, "list"))
  expect_true(inherits(sd$eta, c("data.table", "data.frame")))
})

#------------------- set_data - end --------------------------------------------

#------------------- get_cats - start ------------------------------------------

test_that("get_cats: params NULL result: error missing arguments", {
  expect_error(get_cats())
})

test_that("get_cats: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(get_cats(ctr))
})

test_that("get_cats: params: ctr result: identical inherits", {
  ctr <- theophylline()
  gcats <- get_cats(ctr)
  expect_true(inherits(gcats, "character"))
})

test_that("Can get cats: params: ctr", {
  ctr <- theophylline()
  gcats <- get_cats(ctr)
  expect_identical(gcats, "SEX")
})
#------------------- get_cats - end --------------------------------------------

#------------------- get_strats - start ----------------------------------------
test_that("get_strats: params NULL result: error missing arguments", {
  expect_error(get_strats())
})

test_that("get_strats: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(get_strats(ctr))
})

test_that("get_strats: params: ctr result: identical inherits", {
  ctr <- theophylline()
  gstrats <- get_strats(ctr)
  expect_true(inherits(gstrats, "character"))
})

test_that("Can get strats: params: ctr ", {
  ctr <- theophylline()
  gstrats <- get_strats(ctr)
  expect_identical(gstrats, "STUD")
})
#------------------- get_strats - end ------------------------------------------

#------------------- get_covariates - start ------------------------------------
test_that("get_covariates: params NULL result: error missing arguments", {
  expect_error(get_covariates())
})

test_that("get_covariates: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(get_covariates(ctr))
})

test_that("get_covariates: params: ctr result: identical inherits", {
  ctr <- theophylline()
  gcov <- get_covariates(ctr)
  expect_true(inherits(gcov, "character"))
})

test_that("Can get covariates: params: ctr", {
  ctr <- theophylline()
  gcov <- get_covariates(ctr)
  expect_identical(gcov, c("SEX", "WT0", "AGE0"))
})
#------------------- get_covariates - end --------------------------------------

#------------------- get_conts - start -----------------------------------------

test_that("get_conts: params NULL result: error missing arguments", {
  expect_error(get_conts())
})

test_that("get_conts: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(get_conts(ctr))
})

test_that("get_conts: params: ctr result: identical inherits", {
  ctr <- theophylline()
  gconst <- get_conts(ctr)
  expect_true(inherits(gconst, "character"))
})

test_that("Can get conts: params: ctr", {
  ctr <- theophylline()
  gconst <- get_conts(ctr)
  expect_identical(gconst, c("WT0", "AGE0"))
})
#------------------- get_conts - end -------------------------------------------

#------------------- get_occ - start -------------------------------------------

test_that("get_occ: params NULL result: error missing arguments", {
  expect_error(get_occ())
})

test_that("get_occ: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(get_occ(ctr))
})

test_that("get_occ: params: ctr result: identical inherits", {
  ctr <- theophylline()
  g_occ <- get_occ(ctr)
  expect_true(inherits(g_occ, "character"))
})

test_that("Can get occ: params: ctr", {
  ctr <- theophylline()
  g_occ <- get_occ(ctr)
  expect_identical(g_occ, "")
})
#------------------- get_occ - end ---------------------------------------------

#------------------- pmx_print - start -----------------------------------------
ctr <- pmxClassHelpers$ctr
theophylline <- file.path(
  system.file(package = "ggPMX"), "testdata",
  "theophylline"
)
private <- list(
  .data_path = "",
  .input_path = "",
  .covariates = NULL,
  .plots = list(),
  .plots_configs = list()
)
WORK_DIR <- file.path(theophylline, "Monolix")
input_file <- file.path(theophylline, "data_pk.csv")
self <- pmx(
  sys = "mlx",
  config = "standing",
  directory = WORK_DIR,
  input = input_file,
  dv = "Y",
  dvid = "DVID"
)

test_that("pmx_print: params NULL result: error missing arguments", {
  expect_error(pmx_print())
})

test_that("pmx_print can print pmx class", {
  expect_output(pmx_print(self, private), "pmx object:")
})

test_that("pmx_print  params: self, private; result: identical inherits", {
  expect_true(inherits(pmx_print(self, private), "pmxConfig"))
})


test_that("pmx_shrink: params NULL result: list, pmxShrinkClass", {
  expect_true(inherits(pmx_shrink(), c("list", "pmxShrinkClass")))
})


test_that(
  "pmx_shrink: params: fun, size, color, vjust, hjust
  result: list, pmxShrinkClass", {
  expect_true(
    inherits(
      pmx_shrink(fun="sd", size=1, color="red", vjust=1, hjust=1),
      c("list", "pmxShrinkClass")
    )
  )}
)


test_that("pmx_shrink: params result: elements in the list", {
    sh_names <- c("fun", "size", "color", "vjust", "hjust")
    expect_true(all(sh_names %in% names(pmx_shrink())))
})


test_that("check_shrink: shrink_list result: logical ", {
  expect_true(
    inherits(
      check_shrink(list(fun="sd", size=1, color="red", vjust=1, hjust=1)),
      "logical"
    )
  )
})


test_that("check_shrink: shrink_list result: character ", {
  expect_true(
    inherits(
      check_shrink(list(fun="sd", size=1, color="red")), "character"
    )
  )
})

test_that("pmx_print params: self, private; result: identical structure", {
  pmx_pr <- pmx_print(self, private)
  expect_identical(pmx_pr$sys, "mlx")
  expect_identical(pmx_pr$plots$ABS_IWRES_IPRED$ptype, "SCATTER")
})

#------------------- pmx_print - end -------------------------------------------

#------------------- pmx_transform - start -------------------------------------

test_that("pmx_transform: params NULL result: error missing arguments", {
  expect_error(pmx_transform())
})

#------------------- pmx_transform - end ---------------------------------------

#------------------- pmx_copy - end --------------------------------------------

test_that("pmx_copy: params NULL result: error missing arguments", {
  expect_error(pmx_copy())
})

test_that("pmx_copy: params: ctr result: error ctr is not a pmxClass", {
  ctr <- ""
  expect_error(pmx_copy(ctr))
})


test_that("pmx_copy: params: ctr result: Creates a deep copy of the controller", {
  ctr <- pmxClassHelpers$ctr
  pmxNames <- c(
    "warnings", "update_plot", "time", "strats", "sim_blq",
    "sim", "settings", "set_config", "save_dir", "report_queue",
    "report_n", "remove_plot", "re", "print", "post_load",
    "plots", "plot_file_name", "occ", "input_file", "input",
    "initialize", "id", "has_re", "get_plot", "get_config",
    "footnote", "enqueue_plot", "endpoint", "dvid", "dv",
    "dequeue_plot", "data", "conts", "config", "clone",
    "cats", "bloq", "add_plot", "abbrev", ".__enclos_env__"
  )
  p_copy <- pmx_copy(ctr, is.draft = FALSE)
  expect_identical(names(p_copy), pmxNames)
  expect_identical(p_copy$conts, c("WT0", "AGE0"))
  expect_true(inherits(p_copy, c("pmxClass", "R6")))
})

#------------------- pmx_copy - end --------------------------------------------

#------------------- print.pmxClass - start ------------------------------------

test_that("print.pmxClass: params NULL result: error missing arguments", {
  expect_error(print.pmxClass())
})

test_that("Can print pmxClass: params ctr", {
  ctr <- pmxClassHelpers$ctr
  expect_output(print.pmxClass(ctr), "pmx object:")
})

test_that("print.pmxClass: params ctr is a pmxClass obj; result: identical inherits", {
  ctr <- theophylline()
  expect_true(inherits(print.pmxClass(ctr), "pmxConfig"))
})
#------------------- print.pmxClass - end --------------------------------------

#------------------- pmx_fig_process_wrapup - start ----------------------------

test_that("pmx_fig_process_wrapup: params NULL result: error missing arguments", {
  expect_error(pmx_fig_process_wrapup())
})

test_that("pmx_fig_process_wrapup can wrap up process: params self", {
  expect_true(pmx_fig_process_wrapup(self))
  expect_true(inherits(pmx_fig_process_wrapup(self), "logical"))
})

test_that("pmx_fig_process_wrapup params: self result: error queue is not empty", {
  expect_true(pmx_fig_process_wrapup(self))
  self$report_queue <- TRUE
  expect_error(pmx_fig_process_wrapup(self))
  self$report_queue <- list()
})

#------------------- pmx_fig_process_wrapup - end ------------------------------

#------------------- pmx_fig_process_init - start ------------------------------

test_that("pmx_fig_process_init: params: NULL result: identical inherits", {
  expect_true(inherits(pmx_fig_process_init(), "numeric"))
})

test_that("pmx_fig_process_init: params: self result: identical values", {
  pmx_f <- pmx_fig_process_init(self)
  expect_identical(pmx_f, 0)
})

#------------------- pmx_fig_process_init - end --------------------------------

#------------------- pmx_dequeue_plot - start ----------------------------------

test_that("pmx_dequeue_plot: params: NULL result: error missing arguments", {
  expect_error(pmx_dequeue_plot())
})

test_that("pmx_dequeue_plot: params: self result: warning: ... Footnotes may be wrong", {
  expect_message(pmx_dequeue_plot(self))
})
test_that("pmx_dequeue_plot: params: self result: identical inherits", {
  self$report_queue <- list(1, 2)
  expect_true(inherits(pmx_dequeue_plot(self), "numeric"))
})

test_that("pmx_dequeue_plot can dequeue plot: params: self", {
  self$report_queue <- TRUE
  expect_true(pmx_dequeue_plot(self))
  self$report_queue <- list()
})

#------------------- pmx_dequeue_plot - end ------------------------------------

#------------------- pmx_get_config - start ------------------------------------

test_that("pmx_get_config: params: NULL result: error missing arguments", {
  expect_error(pmx_get_config())
})

#------------------- pmx_get_config - end --------------------------------------

#------------------- pmx_initialize - start ------------------------------------
test_that("pmx_initialize: params NULL result: error expecting source path", {
  expect_error(pmx_initialize())
})
#------------------- pmx_initialize - start ------------------------------------

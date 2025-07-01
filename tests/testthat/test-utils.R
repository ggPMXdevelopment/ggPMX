if (helper_skip()) {
  context("Test utility functions")
  tmp_dir <- tempfile("tmp")
  dir.create(tmp_dir)

  test_that("merge vectors error works", {
    expect_error(
      mergeVectors.(1:4, 5:8),
      "Vectors must be either NULL or have names for all elements"
    )
  })


  test_that("pk_pd is working", {
    tmp <- capture.output(ctr <- pk_pd()) # assign ctr silently
    expect_s3_class(ctr, "pmxClass")
  })


  test_that("pk_pd params: code; result: identical structure", {
    tmp <- capture.output(ctr <- pk_pd(code = "4")) # assign ctr silently
    pk_pd_path <- file.path(
      system.file(package = "ggPMX"), "testdata",
      "pk_pd"
    )
    input_file <- file.path(pk_pd_path, "pk_pd.csv")
    epNames <- c("code", "label", "unit", "file.code", "trans")
    expect_identical(ctr$input_file, input_file)
    expect_identical(names(ctr$endpoint), epNames)
    expect_identical(ctr$endpoint$code, "4")
  })

  #------------------- l_left_join start ----------------------------------
  test_that("l_left_join merge compound lists", {
    res <-
      l_left_join(
        list(
          x = 1,
          y = 1,
          h = list(z = 1)
        ),
        list(
          y = 2,
          h = list(h = 4)
        )
      )
    expected <- list(x = 1, h = list(z = 1, h = 4), y = 2)
    expect_identical(res, expected)
  })


  ## Testing parse_mlxtran below
  file_name <- file.path(
    system.file(package = "ggPMX"),
    "testdata",
    "1_popPK_model",
    "project.mlxtran"
  )

  wd <- file.path(
    system.file(package = "ggPMX"),
    "testdata",
    "1_popPK_model"
  )

  for (f in list.files(path = wd)) {
    if (f != "RESULTS") {
      suppressWarnings(file.copy(file.path(wd, f), file.path(tmp_dir, f), copy.mode = FALSE))
    }
  }
  dir.create(file.path(tmp_dir, "RESULTS"))
  for (f in list.files(path = file.path(wd, "RESULTS"))) {
    suppressWarnings(file.copy(file.path(wd, "RESULTS", f),
                               file.path(tmp_dir, "RESULTS", f),
                               copy.mode = FALSE
                               ))
  }

  wd <- tmp_dir

  mlxpath <- file.path(wd, "project_copy.mlxtran")

  file_name <- file.path(
    wd,
    "project.mlxtran"
  )
  
  test_that("parse_mlxtran: params: no warning", {
    skip("skipping ChartsData export warning test")
    expect_no_warning(
      parse_mlxtran(file_name), 
      message = "ggPMX needs ChartsData exported"
    )
  })

  test_that("parse_mlxtran: params: folder name", {
    a <- suppressWarnings(parse_mlxtran(file_name))
    expect_true(inherits(
      a,
      "list"
    ))
    expect_equal(normalizePath(a$directory), normalizePath(file.path(wd, "RESULTS")))
  })


  test_that("parse_mlxtran: params: full file_name", {
    skip_on_cran()
    dir.create(file.path(wd, "result"))
    section.name <- line <- section <- NULL
    sub_section <- sub_section.name <- NULL
    value <- NULL
    lines <- readLines(file_name)

    firsts <- min(grep("<.*>", lines)) # first section
    lines <- lines[firsts:length(lines)]
    lines <- lines[lines != "" & !grepl(":$", lines)]
    lines[grepl("exportpath = ", lines) == TRUE] <- paste0("exportpath = '", wd, "/result'")

    writeLines(lines, mlxpath)
    a <- suppressWarnings(parse_mlxtran(mlxpath))
    file.remove(mlxpath)
    unlink(file.path(wd, "result"), recursive = TRUE)

    expect_true(inherits(
      a,
      "list"
    ))
    
    # for test purposes doesn't matter if files don't exist
    path_1 <- suppressWarnings(normalizePath(a$directory))
    path_2 <- suppressWarnings(normalizePath(file.path(wd, "result")))
    
    expect_equal(path_1, path_2)
  })


  test_that("parse_mlxtran: params: no exist file_name", {
    dir.create(file.path(wd, "result"))
    section.name <- line <- section <- NULL
    sub_section <- sub_section.name <- NULL
    value <- NULL
    lines <- readLines(file_name)

    firsts <- min(grep("<.*>", lines)) # first section
    lines <- lines[firsts:length(lines)]
    lines <- lines[lines != "" & !grepl(":$", lines)]
    lines[grepl("exportpath = ", lines) == TRUE] <- paste0("exportpath = '", wd, "/result/res'")

    writeLines(lines, mlxpath)
    a <- suppressWarnings(parse_mlxtran(mlxpath))
    file.remove(mlxpath)
    unlink(file.path(wd, "result"), recursive = TRUE)

    expect_true(inherits(
      a,
      "list"
    ))
    expect_equal(normalizePath(a$directory), normalizePath(file.path(wd, "RESULTS")))
  })


  test_that("l_left_join params: NULL result: error file_name is missing", {
    expect_error(l_left_join())
  })


  test_that("l_left_join params: base_list, overlay_list; result: identical structure", {
    default_hline <- list(yintercept = 0)
    hline <- list(yintercept = 1)
    l_join <- l_left_join(default_hline, hline)
    expect_identical(l_join$yintercept, 1)
  })


  test_that("l_left_join params: base_list, overlay_list, recursive = FALSE;
          result: identical structure", {
            default_hline <- list(yintercept = 0)
            hline <- list(yintercept = 1)
            l_join <- l_left_join(default_hline, hline, recursive = FALSE)
            expect_identical(l_join$yintercept, 1)
          })


  test_that("l_left_join params: base_list, overlay_list; result: identical inherits", {
    default_hline <- list(yintercept = 0)
    hline <- list(yintercept = 1)
    l_join <- l_left_join(default_hline, hline)
    expect_true(inherits(l_join, "list"))
  })
  #------------------- l_left_join end ------------------------------------

  #------------------- parse_mlxtran start --------------------------------

  test_that("parse_mlxtran params: NULL result: error file_name is missing", {
    expect_error(parse_mlxtran())
  })


  test_that("parse_mlxtran params: file_name result: error file_name is not a mlxtran file", {
    mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "1_popPK_model", "project.mlxtran1")
    expect_error(parse_mlxtran(file_name = mlxtran_path))
  })


  test_that("parse_mlxtran params: file_name result: error file do not exist", {
    mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "1_popPK_model", "Pr.mlxtran")
    expect_error(parse_mlxtran(file_name = mlxtran_path))
  })


  test_that("parse_mlxtran params: file_name result: identical names", {
    mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "1_popPK_model", "project.mlxtran")
    par <- suppressWarnings(parse_mlxtran(file_name = mlxtran_path))
    parseNames <- c("directory", "input", "dv", "id", "time", "cats", "conts", "occ", "dvid", "endpoint")
    expect_identical(names(par), parseNames)
  })


  test_that("parse_mlxtran params: file_name result: identical inherits", {
    mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "1_popPK_model", "project.mlxtran")
    par <- suppressWarnings(parse_mlxtran(file_name = mlxtran_path))
    expect_true(inherits(par, "list"))
  })


  test_that("parse_mlxtran params: file_name result: identical structure", {
    mlxtran_path <- file.path(system.file(package = "ggPMX"), "testdata", "1_popPK_model", "project.mlxtran")
    par <- suppressWarnings(parse_mlxtran(file_name = mlxtran_path))
    expect_identical(par$cats, c("SEX", "RACE", "DISE", "ILOW"))
    expect_identical(par$endpoint$code, "1")
    expect_identical(par$time, "TIME")
    expect_true(length(par$cats) > 0)
    expect_true(length(par$conts) > 0)
    expect_true(length(par$occ) > 0)
    expect_true(length(par$dvid) > 0)
  })

  #------------------- parse_mlxtran end --------------------------------

  #------------------- quantile start -----------------------------------

  test_that("quantile result: error x is missing", {
    expect_error(quantile())
  })

  #------------------- quantile end -----------------------------------

  #------------------- anyUnnamed. start ------------------------------

  test_that("anyUnnamed. params: NULL result: error x is missing", {
    expect_error(anyUnnamed.())
  })


  test_that("anyUnnamed. params: x result: identical inherits", {
    expect_true(inherits(anyUnnamed.(1:4), "logical"))
  })


  test_that("anyUnnamed. params: NULL result: FALSE", {
    expect_false(anyUnnamed.(NULL))
  })

  test_that("anyUnnamed. params: x; result: TRUE", {
    expect_true(anyUnnamed.(x = NA))
  })

  #------------------- anyUnnamed. end --------------------------------

  #------------------- mergeVectors. start ----------------------------

  test_that("anyUnnamed. params: NULL result: error missing arguments", {
    expect_error(mergeVectors.())
  })

  test_that("merge vectors error works", {
    expect_error(
      mergeVectors.(1:4, 5:8),
      "Vectors must be either NULL or have names for all elements"
    )
  })

  test_that("mergeVectors. params: a,b; result: identical structure", {
    mergeV <- mergeVectors.(a = list(vec = 1), b = list(vec = 4))
    expect_identical(mergeV$vec, 4)
  })

  test_that("mergeVectors. params: a,b; result: identical inherits", {
    mergeV <- mergeVectors.(a = c(vec = 1), b = c(vec = 4))
    expect_true(inherits(mergeV, "numeric"))
  })

  #------------------- mergeVectors. end ------------------------------

  #------------------- local_filter start -----------------------------

  test_that("local_filter params: NULL result: error x is missing", {
    expect_error(local_filter())
  })

  test_that("local_filter params: NULL result: indentical inherits", {
    l_filter <- local_filter("STUD == 1")
    expect_true(inherits(l_filter, "function"))
  })

  #------------------- local_filter end -------------------------------

  #------------------- dropNulls. start --------------------------------

  test_that("dropNulls. params: NULL result: error x is missing", {
    expect_error(dropNulls.())
  })


  test_that("dropNulls. params: x = NULL, y = NULL result: NULL", {
    opt <- dropNulls.(mergeVectors.(NULL, NULL))
    expect_true(is.null(opt))
  })
  if (helper_skip()) {

    #------------------- dropNulls. end --------------------------------

    #------------------- merge_defaults start --------------------------------
    test_that("merge_defaults params: NULL result: error x is missing", {
      expect_error(merge_defaults())
    })


    test_that("merge_defaults params: x, y result: identical vectors and values", {
      expect_equal(merge_defaults(1:4, 5:8), c(1, 2, 3, 4))
      m <- merge_defaults(
        x = list(
          x = 1,
          y = 1,
          h = list(z = 1)
        ),
        y = list(
          y = 2,
          h = list(h = 4)
        )
      )
      expect_equal(m$h$z, 1)
    })

    #------------------- merge_defaults end --------------------------------

    #------------------- is.formula start ----------------------------------------


    test_that("is.formula params: NULL result: error x is missing", {
      expect_error(is.formula())
    })

    test_that("is.formula: params: formula result: formula", {
      x <- ~ a + y + z
      expect_true(is.formula(x))
    })

    test_that("is.formula: params: formula (2) result: formula", {
      x <- y ~ z
      expect_true(is.formula(x))
    })

    test_that("is.formula: params: expression result: not formula", {
      x <- expression(x^2 - 2 * x + 1)
      expect_false(is.formula(x))
    })

    test_that("is.formula: params: integer result: not formula", {
      x <- 10L
      expect_false(is.formula(x))
    })

    test_that("is.formula: params: NULL result: not formula", {
      x <- NULL
      expect_false(is.formula(x))
    })
    #------------------- is.formula end ------------------------------------------

    #------------------- theophylline start --------------------------------------

    test_that("theophylline: params: NULL result: identical inherits", {
      expect_true(inherits(theophylline(), c("pmxClass", "R6")))
    })


    test_that("theophylline: params: settings, result: identical names", {
      ctr <- theophylline(
        settings = pmx_settings(
          effects = list(
            levels = c("ka", "V", "Cl"),
            labels = c("Concentration", "Volume", "Clearance")
          )
        )
      )

      theoNames <- c(
        ".__enclos_env__", "sim_blq", "time", "id", "bloq", "sim",
        "plot_file_name", "report_n", "report_queue", "save_dir", "footnote", "warnings",
        "endpoint", "abbrev", "re", "has_re", "settings", "strats", "occ", "conts",
        "cats", "dvid", "dv", "input_file", "input", "config", "data", "clone",
        "post_load", "plots", "get_plot", "set_config", "get_config", "remove_plot",
        "update_plot", "add_plot", "dequeue_plot", "enqueue_plot", "print", "initialize"
      )

      expect_equal(names(ctr), theoNames)
    })

    test_that("theophylline: params:  settings, result: identical levels and labels", {
      ctr <- theophylline(settings = pmx_settings(
        effects = list(
          levels = c("ka", "V", "Cl"),
          labels = c("Concentration", "Volume", "Clearance")
        )
      ))
      expect_true(file.exists(ctr$save_dir))
      expect_true(inherits(ctr$sim, c("pmxSimClass", "list")))
      expect_identical(ctr$settings$effects$levels, c("ka", "V", "Cl"))
      expect_identical(ctr$settings$effects$labels, c("Concentration", "Volume", "Clearance"))
      expect_true(ctr$settings$use.abbrev)
    })
    #------------------- theophylline end ----------------------------------------

    #------------------- pk_occ start --------------------------------------------

    test_that("pk_occ: params: NULL result: identical inherits", {
      expect_true(inherits(pk_occ(), c("pmxClass", "R6")))
    })


    test_that("pk_occ: params: NULL result: identical structure", {
      ctr <- pk_occ()
      expect_identical(ctr$dvid, "YTYPE")
      expect_identical(ctr$cats, c("SEX", "RACE", "DISE", "ILOW"))
      expect_identical(ctr$conts, c("AGE0", "WT0", "HT0", "TRT"))
      expect_true(inherits(ctr$input, c("data.table", "data.frame")))
      expect_false(ctr$footnote)
    })


    test_that("pk_occ: params: NULL result: identical inherits", {
      ctr <- pk_occ()
      pkNames <- c(
        ".__enclos_env__", "sim_blq", "time", "id", "bloq", "sim", "plot_file_name",
        "report_n", "report_queue", "save_dir", "footnote", "warnings", "endpoint", "abbrev",
        "re", "has_re", "settings", "strats", "occ", "conts", "cats",
        "dvid", "dv", "input_file", "input", "config", "data", "clone",
        "post_load", "plots", "get_plot", "set_config", "get_config", "remove_plot", "update_plot",
        "add_plot", "dequeue_plot", "enqueue_plot", "print", "initialize"
      )
      expect_equal(names(ctr), pkNames)
    })

    #------------------- pk_occ end ----------------------------------------------

    #------------------- abbrev start --------------------------------------------

    test_that("abbrev: params: NULL result: identical inherits", {
      expect_true(inherits(abbrev(), "list"))
    })

    test_that("abbrev: params: param; result: abbreviation term", {
      expect_identical(abbrev("COAR"), "Clinical Operations Analytics and Regions")
    })


    test_that("abbrev: params: NULL result: identical abbrev", {
      abbr <- abbrev()

      abbrNames <- c(
        "AIC", "BIC", "BLQ", "COAR", "DV", "ETA", "EBE", "FO", "FOCE", "FOCEI",
        "IIV", "IPRED", "LRT", "M&S", "NLME", "NPD", "NPDE", "OCP", "OFV", "PD",
        "PK", "PDF", "SAEM", "VPC", "PRED", "EPRED", "CPRED", "IWRES", "|IWRES|",
        "NVS", "HA", "TIME"
      )

      expect_identical(names(abbr), abbrNames)
    })
    #------------------- abbrev end ----------------------------------------------

    unlink(tmp_dir, recursive = TRUE)
  }
}

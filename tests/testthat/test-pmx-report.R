if (helper_skip()) {
  library(rmarkdown)
  library(purrr)
  
  # ---- Setup shared objects ----
  ctr <- theophylline()
  tmp_dir <- tempdir(check = TRUE)
  template_path <- system.file("rmarkdown", "templates", "standing", package = "ggPMX")

  # ---- pmx_fig_process tests ----
  context("Test pmx_fig_process function")

  test_that("pmx_fig_process: returns character and correct old_name", {
    out <- pmx_fig_process(ctr, old_name = "gg", footnote = TRUE, out_ = tmp_dir)
    expect_true(inherits(out, "character"))
    expect_identical(out, "gg")
  })

  test_that("pmx_fig_process: error on missing params", {
    expect_error(pmx_fig_process())
  })

  # ---- pmx_draft tests ----
  context("Test pmx_draft function")

  test_that("pmx_draft: error if ctr missing", {
    expect_error(pmx_draft())
  })

  test_that("pmx_draft: error if template does not exist", {
    expect_error(pmx_draft(ctr,
      name = "dr_Report_ggPMX",
      template = system.file("rm", package = "ggPMX"),
      edit = FALSE
    ))
  })

  test_that("pmx_draft: returns character, creates and removes file", {
    draft <- pmx_draft(ctr,
      name = "dr_Report_ggPMX",
      template = template_path,
      edit = FALSE
    )
    expect_true(inherits(draft, "character"))
    expect_true(file.exists(draft))
    file.remove(draft)
    expect_false(file.exists(draft))
  })

  # ---- create_ggpmx_gof tests ----
  context("Test create_ggpmx_gof function")
  test_that("create_ggpmx_gof: error on missing name", {
    expect_error(create_ggpmx_gof())
  })

  # ---- rm_dir tests ----
  context("Test rm_dir function")
  test_that("rm_dir: error on missing param", {
    expect_error(rm_dir())
  })
  test_that("rm_dir: removes directory", {
    skip_on_cran()
    work_dir <- file.path(tmp_dir, "create_ggpmx_gof_test")
    dir.create(work_dir)
    rm_dir(work_dir)
    expect_false(dir.exists(work_dir))
  })

  # ---- remove_reports tests ----
  context("Test remove_reports function")
  test_that("remove_reports: removes report files", {
    skip_on_cran()
    work_dir <- file.path(tmp_dir, "report_plot")
    dir.create(work_dir, showWarnings = FALSE)
    ctr %>% pmx_report(
      name = "Report_ggPMX",
      save_dir = work_dir,
      template = template_path,
      output = "all",
      format = "word"
    )
    expect_true(file.exists(file.path(work_dir, "Report_ggPMX.docx")))
    remove_reports(output = "plots", work_dir)
    expect_false(file.exists(file.path(work_dir, "Report_ggPMX.docx")))
  })

  # ---- pmx_report tests ----
  context("Test pmx_report function")

  test_that("pmx_report: error on invalid save_dir", {
    skip_on_cran()
    expect_error(ctr %>% pmx_report(
      name = "Report_ggPMX",
      save_dir = NULL,
      template = template_path,
      output = "report",
      format = "html"
    ))
    expect_error(ctr %>% pmx_report(
      name = "Report_ggPMX",
      save_dir = file.path(tmp_dir, "nonexistent_dir"),
      template = template_path,
      output = "report",
      format = "html"
    ))
  })

  # ---- Single multi-output test per mode ----
  test_that("pmx_report: creates expected files for all output modes (minimal set)", {
    skip_on_cran()
    work_dir <- file.path(tmp_dir, "pmx_report_test")
    dir.create(work_dir, showWarnings = FALSE)
    on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

    # Only run once per output type, one format per output for speed!
    ctr %>% pmx_report(
      name = "Report_ggPMX",
      save_dir = work_dir,
      template = template_path,
      output = "all",
      format = "html"
    )
    expect_true(any(grepl("Report_ggPMX", list.files(work_dir))))
    expect_true(any(grepl("html", list.files(work_dir))))
    expect_true(file.exists(file.path(work_dir, "ggpmx_GOF")))

    unlink(list.files(work_dir, full.names = TRUE), recursive = TRUE)
    ctr %>% pmx_report(
      name = "Report_ggPMX",
      save_dir = work_dir,
      template = template_path,
      output = "plots",
      format = "html"
    )
    expect_true(file.exists(file.path(work_dir, "ggpmx_GOF")))

    unlink(list.files(work_dir, full.names = TRUE), recursive = TRUE)
    ctr %>% pmx_report(
      name = "Report_ggPMX",
      save_dir = work_dir,
      template = template_path,
      output = "report",
      format = "html"
    )
    expect_true(any(grepl("Report_ggPMX", list.files(work_dir))))
  })

  # ---- Custom template test (optional, can skip if coverage not needed) ----
  test_that("pmx_report: can use custom template", {
    skip_on_cran()
    custom_template_file <- system.file(
      file.path("examples", "templates", "custom_report.Rmd"),
      package = "ggPMX"
    )
    expect_null(
      pmx_report(
        contr = ctr,
        name = "NN",
        save_dir = tmp_dir,
        template = custom_template_file
      )
    )
  })

  # ---- Error cases for illegal arguments ----
  test_that("pmx_report: illegal arguments throw errors", {
    skip_on_cran()
    expect_error(pmx_report(contr = list(), name = "Report_ggPMX", save_dir = tmp_dir, format = "html", output = "all"))
    expect_error(pmx_report(contr = list(), name = c("a", "b"), save_dir = tmp_dir, output = "all", format = "html"))
    expect_error(pmx_report(contr = list(), name = "a", save_dir = tmp_dir, output = "all", format = "html", template = 1L))
  })

  # ---- nlmixr controller test ----
  context("Test pmx_nlmixr controller")
  if (requireNamespace("nlmixr2est", quietly=TRUE)) {
    skip_on_os("windows") 
    one.compartment <- function() {
      ini({
        tka <- 0.45 # Log Ka
        tcl <- 1 # Log Cl
        tv <- 3.45 # Log V
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }
    fit <- nlmixr2est::nlmixr(one.compartment, nlmixr2data::theo_sd, "saem", control = list(print = 0))
    nlmixr_ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))
    work_dir <- file.path(tmp_dir, "pmx_nlmixr_report")
    dir.create(work_dir)
    test_that("pmx_report: works with nlmixr controller", {
      skip_on_cran()
      expect_error(nlmixr_ctr %>% pmx_report(
        name = "Report_ggPMX",
        save_dir = work_dir,
        template = template_path,
        output = "all",
        format = "html"
      ), NA)
      unlink(work_dir, recursive = TRUE)
    })
  }
}
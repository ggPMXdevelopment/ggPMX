if (helper_skip()) {
  library(rmarkdown)
  library(purrr)


  #------------------- pmx_fig_process - start -----------------------------------
  ctr <- theophylline()

  context("Test pmx_fig_process function")
  test_that("pmx_fig_process: params: ctr, old_name, footnote, out_ result: identical inherits", {
    tmp_dir <- tempdir(check = TRUE)
    pmx_fig <- pmx_fig_process(ctr, old_name = "gg", footnote = TRUE, out_ = tmp_dir)
    expect_true(inherits(pmx_fig, "character"))
  })

  test_that("pmx_fig_process: params: ctr, old_name, footnote, out_ result: identical old_name", {
    tmp_dir <- tempdir(check = TRUE)
    pmx_fig <- pmx_fig_process(ctr, old_name = "gg", footnote = TRUE, out_ = tmp_dir)
    expect_identical(pmx_fig, "gg")
  })


  test_that("pmx_fig_process: params: NULL result: footnote is missing", {
    expect_error(pmx_fig_process())
  })

  #------------------- pmx_fig_process - end -------------------------------------

  #------------------- pmx_draft - start -----------------------------------------
  test_that("pmx_draft: params: NULL result: ctr is missing", {
    expect_error(pmx_draft())
  })


  test_that("pmx_draft: params  result: error template do not exist", {
    expect_error(pmx_draft(ctr,
                           name = "dr_Report_ggPMX", template =
                                                       system.file("rm", package = "ggPMX"),
                           edit = FALSE
                           ))
  })

  test_that("pmx_draft: params  result: identical inherits", {
    draft <- pmx_draft(ctr,
                       name = "dr_Report_ggPMX", template =
                                                   system.file("rmarkdown", "templates",
                                                               "standing",
                                                               package = "ggPMX"
                                                               ),
                       edit = FALSE
                       )
    expect_true(inherits(draft, "character"))
    file.remove(draft)
  })

  test_that("pmx_draft: params  result: create file", {
    draft <- pmx_draft(ctr,
                       name = "dr_Report_ggPMX", template =
                                                   system.file("rmarkdown", "templates",
                                                               "standing",
                                                               package = "ggPMX"
                                                               ),
                       edit = FALSE
                       )
    expect_true(file.exists(draft))
    file.remove(draft)
    expect_false(file.exists(draft))
  })

  #------------------- pmx_draft - end -------------------------------------------

  #------------------- create_ggpmx_gof - start ----------------------------------
  context("Test create_ggpmx_gof")
  test_that("create_ggpmx_gof params NULL result: error name is missing", {
    expect_error(create_ggpmx_gof())
  })

  #------------------- create_ggpmx_gof - end ------------------------------------

  #------------------- rm_dir - end ----------------------------------------------
  context("Test rm_dir")
  test_that("rm_dir params NULL result: error to_remove is missing", {
    expect_error(rm_dir())
  })

  test_that("rm_dir params NULL result: remove directory", {
    path <- file.path(tempdir(check = TRUE))
    newfolder <- "create_ggpmx_gof_test"
    work_dir <- file.path(path, newfolder)
    dir.create(work_dir)
    rm_dir(work_dir)
    expect_false(dir.exists(work_dir))
  })

  #------------------- rm_dir - end ----------------------------------------------

  #------------------- remove_reports - start ------------------------------------
  test_that("remove_reports params result: remove plot files", {
    path <- file.path(tempdir(check = TRUE))
    newfolder <- "report_plot"
    work_dir <- file.path(path, newfolder)
    dir.create(work_dir)

    ctr %>% pmx_report(
      name = "Report_ggPMX",
      save_dir = work_dir,
      template = system.file(
        "rmarkdown", "templates",
        "standing",
        package = "ggPMX"
      ),
      output = "all",
      format = "all"
    )
    files_word_report <- list.files(path = work_dir, pattern = "docx")
    expect_false(is_empty(files_word_report))
    remove_reports(output = "plots", work_dir)
    files_word_report <- list.files(path = work_dir, pattern = "docx")
    expect_true(is_empty(files_word_report))
  })

  #------------------- remove_reports - end --------------------------------------

  # ------------------- pmx_report - generation of report - start ----------------
  context("Test generation of report from a pre-defined template")

  ctr <- test_pmxClass_helpers()[["ctr"]]
  tmp_dir <- tempdir(check = TRUE)

  testGenerateReport <- function() {
    pmx_report(
      contr=ctr,
      name="Report_ggPMX",
      save_dir=tmp_dir,
      output="all",
      format="all"
    )
  }


  test_that("Can generate report", {
    skip_on_cran()
    expect_is(ctr, "pmxClass")
    testGenerateReport()
    list_of_rep <- list.files(path = tmp_dir, pattern = "Report_ggPMX\\..*")
    expect_equal(length(list_of_rep), 4L)
  })


  test_that("Report generation can be repeated without error", {
    skip_on_cran()
    expect_is(ctr, "pmxClass")

    expect_error(
      lapply(1:3, function(n) testGenerateReport()),
      NA
    )
  })

  test_that("Can generate report using a custom template", {
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

  test_that("Illegal arguments to pmx_report cause an error", {
    skip_on_cran()
    expect_is(ctr, "pmxClass")

    expect_error(
      pmx_report(
        contr=list(),
        name="Report_ggPMX",
        save_dir=tmp_dir,
        format="all",
        output="all"
      )
    )

    expect_error(
      pmx_report(
        contr=list(),
        name=c("a", "b"),
        save_dir=tmp_dir,
        output="all",
        format="all"
      )
    )

    expect_error(
      pmx_report(
        contr=list(),
        name="a",
        save_dir=tmp_dir,
        output="all",
        format="all",
        template=1L
      )
    )
  })
  #------------------- pmx_report - generation of report - end -------------------

  library(purrr)
  #------------------- pmx_report - generation of report - end -------------------

  ctr <- theophylline()

  #------------------- pmx_report - output - "all" - start -----------------------

  context("Test pmx_report function with all output")
  test_that("pmx_report: params: ctr, name, save_dir, output, format;
         result: create all files", {
           skip_on_cran()
           path <- file.path(tempdir(check=TRUE))
           newfolder <- "pmx_report_test"
           work_dir <- file.path(path, newfolder)
           dir.create(work_dir, showWarnings=FALSE)

           ctr %>% pmx_report(
             name="Report_ggPMX",
             save_dir=work_dir,
             template=system.file("rmarkdown", "templates", "standing", package="ggPMX"),
             output="all",
             format=c("word", "html")
           )

           files_word_all <- list.files(path=work_dir, pattern="docx")
           files_html_all <- list.files(path=work_dir, pattern="html")
           files_report_ggPMX <- list.files(path=work_dir, pattern="Report_ggPMX")
           sub_dir <- file.path(work_dir, "ggpmx_GOF")


           expect_false(is_empty(files_report_ggPMX))
           expect_true(file.exists(sub_dir))
           expect_false(is_empty(sub_dir))
           expect_false(is_empty(files_word_all))
           expect_false(is_empty(files_html_all))
           unlink(work_dir, recursive=TRUE)
         })

  #------------------- pmx_report - output - "all" - end -------------------------

  #------------------- pmx_report - output - "plots" - start ---------------------

  context("Test pmx_report function with plots output")
  test_that("pmx_report: params: ctr, name, save_dir, output, format; result: create plots files",{
    skip_on_cran()
    path <- file.path(tempdir(check=TRUE))
    newfolder <- "pmx_report_test"
    work_dir <- file.path(path, newfolder)
    dir.create(work_dir, showWarnings=FALSE)

    ctr %>% pmx_report(
      name="Report_ggPMX",
      save_dir=work_dir,
      template=system.file("rmarkdown", "templates", "standing", package="ggPMX"),
      output="plots",
      format=c("word", "html")
    )

    files_word_plots <- list.files(path=work_dir, pattern="docx")
    files_html_plots <- list.files(path=work_dir, pattern="html")
    files_report_ggPMX <- list.files(path=work_dir, pattern="Report_ggPMX")
    sub_dir <- file.path(work_dir, "ggpmx_GOF")
    expect_false(is_empty(files_report_ggPMX))
    expect_true(file.exists(sub_dir))
    expect_false(is_empty(sub_dir))
    expect_true(is_empty(files_word_plots))
    expect_false(is_empty(files_html_plots))

    unlink(work_dir, recursive=TRUE)
  })

  #------------------- pmx_report - "plots" - end --------------------------------

  #------------------- pmx_report - "report" - start -----------------------------

  context("Test pmx_report function with report output")
  test_that("pmx_report: params: ctr, name, save_dir, output, format;
         result: create report files", {
           skip_on_cran()
           path <- file.path(tempdir(check=TRUE))
           newfolder <- "pmx_report_test"
           work_dir <- file.path(path, newfolder)
           dir.create(work_dir, showWarnings=FALSE)

           ctr %>% pmx_report(
             name="Report_ggPMX",
             save_dir=work_dir,
             template=system.file("rmarkdown", "templates", "standing", package="ggPMX"),
             output="report",
             format=c("word", "html")
           )

           files_word_report <- list.files(path=work_dir, pattern="docx")
           files_html_report <- list.files(path=work_dir, pattern="html")
           files_report_ggPMX <- list.files(path=work_dir, pattern="Report_ggPMX")
           sub_dir <- file.path(work_dir, "ggpmx_GOF")
           path <- file.path(tempdir(check = TRUE))
           newfolder <- "pmx_report_test"
           work_dir <- file.path(path, newfolder)
           dir.create(work_dir)
           ctr %>% pmx_report(
             name = "Report_ggPMX",
             save_dir = work_dir,
             template = system.file(
               "rmarkdown", "templates",
               "standing",
               package = "ggPMX"
             ),
             output = "report",
             format = c("word", "html")
           )

           files_word_report <- list.files(path = work_dir, pattern = "docx")
           files_html_report <- list.files(path = work_dir, pattern = "html")
           files_report_ggPMX <- list.files(path = work_dir, pattern = "Report_ggPMX")
           sub_dir <- file.path(work_dir, "ggpmx_GOF")


           expect_false(is_empty(files_report_ggPMX))
           expect_false(file.exists(sub_dir))
           expect_false(is_empty(sub_dir))
           expect_false(is_empty(files_word_report))
           expect_false(is_empty(files_html_report))
           unlink(work_dir, recursive=TRUE)
         })

  #------------------- pmx_report - "report" - end -------------------------------

  #------------------- pmx_report - "all", "plots", "report" - start -------------

  context("Test pmx_report function with all, plots and report output")
  test_that("pmx_report: params: ctr, name, save_dir, output, format;
         result: create all, plots, report files", {
           skip_on_cran()
           path <- file.path(tempdir(check=TRUE))
           newfolder <- "pmx_report_test"
           work_dir <- file.path(path, newfolder)
           dir.create(work_dir, showWarnings=FALSE)

           ctr %>% pmx_report(
             name="Report_ggPMX",
             save_dir=work_dir,
             template=system.file("rmarkdown", "templates", "standing", package="ggPMX"),
             output=c("all", "plots", "report"),
             format=c("word", "html")
           )

           files_word_report <- list.files(path=work_dir, pattern="docx")
           files_html_report <- list.files(path=work_dir, pattern="html")
           files_report_ggPMX <- list.files(path=work_dir, pattern="Report_ggPMX")
           sub_dir <- file.path(work_dir, "ggpmx_GOF")
           expect_false(is_empty(files_report_ggPMX))
           expect_true(file.exists(sub_dir))
           expect_false(is_empty(sub_dir))
           expect_false(is_empty(files_word_report))
           expect_false(is_empty(files_html_report))
           unlink(work_dir, recursive=TRUE)
         })

  #------------------- pmx_report - "all", "plots", "report" - end ---------------

  #------------------- pmx_report  - start ---------------------------------------

  context("Test pmx_report function")

  test_that("pmx_report: params: ctr, name, save_dir = NULL, output, format;
         result: error save directory is not valid", {
           skip_on_cran()
           expect_error(ctr %>% pmx_report(
             name = "Report_ggPMX",
             save_dir = NULL,
             template = system.file(
               "rmarkdown", "templates",
               "standing",
               package = "ggPMX"
             ),
             output = c("all", "plots", "report"),
             format = c("word", "html")
           ))
         })

  test_that("pmx_report: params: ctr, name, save_dir, output, format;
         result: error save directory is not valid(save_dir is not exist)", {
           skip_on_cran()
           tmp_dir <- tempdir(check = TRUE)
           work_dir <- file.path(tmp_dir, "pmx_report_test")
           expect_error(ctr %>% pmx_report(
             name = "Report_ggPMX",
             save_dir = work_dir,
             template = system.file(
               "rmarkdown", "templates",
               "standing",
               package = "ggPMX"
             ),
             output = c("all", "plots", "report"),
             format = "all"
           ))
         })

  #------------------- pmx_report - end ------------------------------------------

  #------------------- pmx_report with nlmixr controller - start -----------------

  context("Test pmx_nlmixr controller")
  if (requireNamespace("nlmixr2", quietly=TRUE)) {
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

    fit <- nlmixr2::nlmixr(one.compartment, nlmixr2data::theo_sd, "saem", control = list(print = 0))
    ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))

  }

  #------------------- pmx_report nlmixr - output - "all" - start ----------------

  context("Test pmx_report function with all output")
  test_that("pmx_report: params: ctr, name, save_dir, output, format;
         result: ok (with proper args)", {
           skip_on_cran()
           path <- file.path(tempdir(check = TRUE))
           newfolder <- "pmx_report_test"
           work_dir <- file.path(path, newfolder)
           dir.create(work_dir)
           expect_error(ctr %>% pmx_report(
             name = "Report_ggPMX",
             save_dir = work_dir,
             template = system.file(
               "rmarkdown", "templates",
               "standing",
               package = "ggPMX"
             ),
             output = "all",
             format = "all"
           ), NA)
           unlink(work_dir, recursive = TRUE)
         })

  #------------------- pmx_report nlmixr - output - "all" - end ------------------
}

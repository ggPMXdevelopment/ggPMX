context("Test generation of report from a pre-defined template")

ctr <- test_pmxClass_helpers()[["ctr"]]
tmp_dir <- tempdir(check=TRUE)

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
  list_of_rep <- list.files(path=tmp_dir,pattern = "Report_ggPMX\\..*")
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
    package="ggPMX"
  )

  expect_null(
    pmx_report(
      contr=ctr,
      name="NN",
      save_dir=tmp_dir,
      template=custom_template_file
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
test_that("pmx_report: params: ctr, name, save_dir, output, format;
         result: create plots files",{
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

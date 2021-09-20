context("Test generation of report from a pre-defined template")

ctr <- test_pmxClass_helpers()[["ctr"]]
tmp_dir <- tempdir(check=TRUE)

testGenerateReport <- function() {
  pmx_report(
    contr=ctr,
    name="Report_ggPMX",
    save_dir=tmp_dir,
    extension="all",
    format="both"
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


test_that("Illegal arguments to pmx_report cause an error", {
  skip_on_cran()
  expect_is(ctr, "pmxClass")

  expect_error(
    pmx_report(
      contr=list(),
      name="Report_ggPMX",
      save_dir=tmp_dir,
      extension="all",
      format="both"
    )
  )

  expect_error(
    pmx_report(
      contr=list(),
      name=c("a", "b"),
      save_dir=tmp_dir,
      extension="all",
      format="both"
    )
  )

  expect_error(
    pmx_report(
      contr=list(),
      name="a",
      save_dir=tmp_dir,
      extension="all",
      format="both",
      template=1L
    )
  )
})

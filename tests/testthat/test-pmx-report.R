context("Test generation of report from a pre-defined template")
pmxClassHelpers <- test_pmxClass_helpers()

test_that("can generate report", {
  skip_on_os("solaris")
  ctr <- pmxClassHelpers$ctr
  expect_is(ctr, "pmxClass")
  tmp_dir <- tempdir(check=TRUE)

  pmx_report(
    contr=ctr,
    name="Report_ggPMX",
    save_dir=tmp_dir,
    extension="all",
    format="both"
  )

  list_of_rep <- list.files(path=tmp_dir,pattern = "Report_ggPMX\\..*")
  expect_equal(length(list_of_rep), 4L)
})

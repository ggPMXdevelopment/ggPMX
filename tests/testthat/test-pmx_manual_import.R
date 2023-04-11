context("Test pmx_manual_import")

#-------------pmx_manual_import START ------------------------------------------
test_that("pmx_manual_import: params: tab_suffix, sim_suffix, tab_names;
          result: identical structure", {
  expect_identical(
    pmx_manual_nm_import("sdtab"),
    list(tab_suffix = "", sim_suffix = "sim", tab_names = "sdtab")
  )
})

test_that("pmx_manual_import: params: tab_suffix, sim_suffix, tab_names;
          result: list", {
  expect_true(inherits(
    pmx_manual_nm_import("sdtab"),
    "list"
  ))
})

test_that("pmx_manual_import: params: no;
          result: identical structure", {
  expect_identical(
    pmx_manual_nm_import(),
    list(
      tab_suffix = "", sim_suffix = "sim",
      tab_names = c(
        "sdtab", "mutab", "patab", "catab",
        "cotab", "mytab", "extra", "xptab",
        "cwtab"
      )
    )
  )
})
#-------------pmx_manual_import END --------------------------------------------

#-------------pmx_list_nm_tables_manual START ----------------------------------
tab_list <- pmx_manual_nm_import(("sdtab"))
runno <- "001"
dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")

test_that("pmx_list_nm_tables_manual: params: runno, file, dir, tab_list;
          result: list", {
  s <- pmx_list_nm_tables_manual(
    runno = runno, file = "run001.lst",
    dir = dir, tab_list
  )
  expect_true(inherits(s, "nm_table_list"))
  expect_true(typeof(s) == "list")
})

test_that("pmx_list_nm_tables_manual: params: runno = NULL, file, dir, tab_list;
          result: identical structure", {
  s <- pmx_list_nm_tables_manual(
    runno = NULL, file = "run001.lst",
    dir = dir, tab_list
  )
  expect_identical(names(s), c("problem", "file", "firstonly", "simtab"))
})

test_that("pmx_list_nm_tables_manual: params: runno, file = NULL, dir, tab_list;
          result: identical structure", {
  s <- pmx_list_nm_tables_manual(
    runno = runno, file = NULL,
    dir = dir, tab_list
  )
  expect_identical(names(s), c("problem", "file", "firstonly", "simtab"))
})

test_that("pmx_list_nm_tables_manual: params: runno, file, dir = NULL, tab_list;
          result: no rows", {
  s <- pmx_list_nm_tables_manual(
    runno = runno, file = "run001.lst",
    dir = NULL, tab_list
  )
  expect_identical(names(s), c("problem", "file", "firstonly", "simtab"))
  expect_equal(nrow(s), 0)
})

test_that("pmx_list_nm_tables_manual: params: runno, file, dir, tab_list;
          result: identical values", {
  s <- pmx_list_nm_tables_manual(
    runno = runno, file = "run001.lst",
    dir = dir, tab_list
  )
  expect_identical(s$problem, 1)
  expect_identical(s$file, file.path(dir, paste0(tab_list$tab_names, runno)))
  expect_false(s$firstonly)
  expect_false(s$simtab)
})
#-------------pmx_list_nm_tables_manual END ------------------------------------

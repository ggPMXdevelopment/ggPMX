context("Test pmx_list_nm_tables")

dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
model_code <- pmx_read_nm_model(
  file = "run001.lst",
  dir = dir
)

#-------------pmx_list_nm_tables START -----------------------------------------
test_that("pmx_manual_import: params: nm_model object;
          result: identical class and structure", {
  s <- pmx_list_nm_tables(model_code)
  expect_true(inherits(s, "nm_table_list"))
  expect_true(typeof(s) == "list")
  expect_true(all(names(s) %in% c("problem", "file", "firstonly", "simtab")))
})

test_that("pmx_list_nm_tables: params: no; result: error", {
  expect_error(pmx_list_nm_tables())
})
#-------------pmx_list_nm_tables END -------------------------------------------

#-------------pmx_is.nm.model START --------------------------------------------
test_that("pmx_is.nm.model: params: nm_model object; result: TRUE", {
  expect_true(pmx_is.nm.model(model_code))
})

test_that("pmx_is.nm.model: params: nm_model object; result: TRUE", {
  ctr <- theophylline()
  expect_false(pmx_is.nm.model(ctr))
})
#-------------pmx_is.nm.model END ----------------------------------------------

#-------------pmx_as.nm.table.list START ---------------------------------------
test_that("pmx_as.nm.table.list: params: nm_model object; result: TRUE", {
  s <- pmx_as.nm.table.list(model_code)
  expect_true(inherits(s, "nm_table_list"))
})

test_that("pmx_as.nm.table.list: params: theophylline; result: TRUE", {
  ctr <- theophylline()
  expect_true(inherits(pmx_as.nm.table.list(ctr), "nm_table_list"))
})
#-------------pmx_as.nm.table.list END -----------------------------------------

#-------------pmx_read_nm_model START ------------------------------------------
test_that("pmx_read_nm_model: params: nm_model object; result: TRUE", {
  expect_true(inherits(model_code, "nm_model"))
  expect_true(typeof(model_code) == "list")
})

test_that("pmx_read_nm_model: params: nm_model object; result: identical structure", {
  expect_true(all(names(model_code) %in% c(
    "problem", "level", "subroutine",
    "code", "comment"
  )))
})
test_that("pmx_read_nm_model: params: no; result: error", {
  expect_error(pmx_read_nm_model())
})
#-------------pmx_read_nm_model END --------------------------------------------




#-------------pmx_is.nm.table.list START ---------------------------------------
test_that("pmx_is.nm.table.list: params: nm_table_list object; result: TRUE", {
  m <- pmx_as.nm.table.list(model_code)
  expect_true(pmx_is.nm.table.list(m))
  expect_true(inherits(pmx_is.nm.table.list(m), "logical"))
})
#-------------pmx_is.nm.table.list END -----------------------------------------

#-------------pmx_file_path START ----------------------------------------------
test_that("pmx_file_path: params: dir, file; result: TRUE", {
  file <- "run001.lst"
  r <- pmx_file_path(dir, file)
  expect_true(nchar(r) > 0)
  expect_true(inherits(r, "character"))
})

test_that("pmx_file_path: params: dir is NULL, file; result: TRUE", {
  file <- "run001.lst"
  r <- pmx_file_path(dir = NULL, file)
  expect_true(r == file)
  expect_true(inherits(r, "character"))
})
#-------------pmx_file_path END ------------------------------------------------

#-------------pmx_tidyr_new_interface START ------------------------------------
test_that("pmx_tidyr_new_interface: params: -; result: TRUE", {
  expect_true(inherits(pmx_tidyr_new_interface(), "logical"))
})
#-------------pmx_tidyr_new_interface END --------------------------------------

#-------------pmx_get_extension START ------------------------------------------
test_that("pmx_get_extension: params: filename; result: TRUE", {
  file <- "run001.lst"
  expect_true(pmx_get_extension(file) == ".lst")
  expect_true(inherits(pmx_get_extension(file), "character"))
})

test_that("pmx_get_extension: params: ''; result: TRUE", {
  file <- ""
  expect_true(pmx_get_extension(file) == "")
  expect_true(inherits(pmx_get_extension(file), "character"))
})

test_that("pmx_get_extension: params: NULL; result: error", {
  expect_error(pmx_get_extension())
})
#-------------pmx_get_extension END --------------------------------------------

#-------------pmx_make_extension START -----------------------------------------
test_that("pmx_make_extension: params: ext; result: TRUE", {
  ext <- ".lst"
  expect_true(pmx_make_extension(ext) == ".lst")
  expect_true(inherits(pmx_make_extension(ext), "character"))
})

test_that("pmx_make_extension: params: vector; result: TRUE", {
  ext <- c("lst", ".txt")
  expect_true(length(pmx_make_extension(ext)) == 2)
  expect_true(pmx_make_extension(ext)[1] == ".lst")
  expect_true(inherits(pmx_make_extension(ext), "character"))
})

test_that("pmx_make_extension: params: NULL; result: error", {
  expect_error(pmx_make_extension())
})
#-------------pmx_make_extension END -------------------------------------------

#-------------pmx_make_extension START -----------------------------------------
test_that("pmx_update_extension: params: x, ext are strings; result: TRUE", {
  x <- "001.lst"
  ext <- ".txt"
  expect_true(pmx_update_extension(x, ext) == "001.txt")
  expect_true(inherits(pmx_update_extension(x, ext), "character"))
})

test_that("pmx_update_extension: params: x, ext is empty; result: TRUE", {
  x <- "001.txt"
  ext <- ""
  expect_true(pmx_update_extension(x, ext) == "001")
})

test_that("pmx_update_extension: params: NULL; result: error", {
  expect_error(pmx_update_extension())
})
#-------------pmx_update_extension END -----------------------------------------

#-------------pmx_msg START ----------------------------------------------------
test_that("pmx_msg: params: txt, quiet is T; result: TRUE", {
  m <- "Message"
  expect_message(pmx_msg(txt = m, quiet = F))
})
#-------------pmx_msg END ------------------------------------------------------

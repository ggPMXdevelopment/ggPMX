context("Test start up settings")

test_that("start up otions are set", {
  pmxOptions(work_dir = "/no/dir")
  pmxOptions(template_dir = "/no/dir2")
  ggPMX:::.onLoad()  
  pmx_options <- pmxOptions()
  expect_identical(pmx_options$work_dir, "/home/agstudy")
  expect_identical(pmx_options$template_dir, 
                   file.path(find.package("ggPMX"), "ggPMX", "templates"))
})

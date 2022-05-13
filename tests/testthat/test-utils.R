context("Test utility functions")

test_that("merge vectors error works", {
  expect_error(
    mergeVectors.(1:4, 5:8),
    "Vectors must be either NULL or have names for all elements"
  )
})

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

test_that("pk_pd is worrking", {
  ctr <- pk_pd()
  expect_s3_class(ctr, "pmxClass")
})


## Testing parse_mlxtran below

file_name <- file.path(system.file(package = "ggPMX"),
                       "testdata",
                       "1_popPK_model",
                       "project.mlxtran")

wd <- file.path(system.file(package = "ggPMX"),
                "testdata",
                "1_popPK_model")

mlxpath <- file.path(wd, "project_copy.mlxtran")

test_that("parse_mlxtran: params: folder name", {
  a <- parse_mlxtran(file_name)
  expect_true(inherits(
    a,
    "list"
  ))
  expect_true(a$directory == file.path(wd, "RESULTS"))
})

test_that("parse_mlxtran: params: full file_name", {
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
  a <- parse_mlxtran(mlxpath)
  file.remove(mlxpath)
  unlink(file.path(wd, "result"), recursive = TRUE)

  expect_true(inherits(
    a,
    "list"
  ))
  expect_true(a$directory == file.path(wd, "result"))
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
  a <- parse_mlxtran(mlxpath)
  file.remove(mlxpath)
  unlink(file.path(wd, "result"), recursive = TRUE)

  expect_true(inherits(
    a,
    "list"
  ))
  expect_true(a$directory == file.path(wd, "RESULTS"))
})

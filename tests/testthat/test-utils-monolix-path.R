test_that("Monolix data paths preserve legacy and modern declarations", {
  directory <- withr::local_tempdir()
  dir.create(file.path(directory, "RESULTS", "ChartsData"), recursive = TRUE)
  dir.create(file.path(directory, "data"))
  project <- file.path(directory, "project.mlxtran")
  for (filename in c(
    "data.csv",
    "data with spaces.csv",
    "data=one.csv",
    "patient's data.csv"
  )) {
    input <- file.path(directory, "data", filename)
    writeLines("id,time,dv\n1,0,1", input)
    relative <- paste0("data/", filename)
    paths <- c(relative, normalizePath(input))
    for (path in paths) {
      quotes <- if (grepl("'", path, fixed = TRUE)) '"' else c("'", '"')
      for (quote in quotes) {
        quoted <- paste0(quote, path, quote)
        declarations <- c(
          quoted,
          paste0("{path=", quoted, "}"),
          paste0("{ path = ", quoted, " }"),
          paste0("{path=", quoted, ", delimiter=','}")
        )
        for (declaration in declarations) {
          writeLines(
            c(
              "<DATAFILE>",
              paste0("file = ", declaration),
              "[CONTENT]",
              "id = {use=identifier}",
              "time = {use=time}",
              "dv = {use=observation, type=continuous}",
              "<FIT> representative",
              "exportpath = 'RESULTS'"
            ),
            project
          )
          before <- getwd()
          parsed <- parse_mlxtran(project)
          expect_identical(
            parsed$input,
            normalizePath(input),
            info = declaration
          )
          expect_identical(parsed$id, "id")
          expect_identical(parsed$dv, "dv")
          expect_identical(getwd(), before)
        }
      }
    }
  }
})

test_that("the bundled legacy theophylline controller still works", {
  ctr <- theophylline()
  expect_s3_class(ctr, "pmxClass")
  expect_gt(nrow(get_data(ctr, "predictions")), 0L)
  expect_s3_class(pmx_plot_dv_pred(ctr), "ggplot")
})

test_that("modern unnamed observation endpoints select the first code", {
  directory <- withr::local_tempdir()
  dir.create(file.path(directory, "RESULTS", "ChartsData"), recursive = TRUE)
  writeLines("id,time,dv,dvid\n1,0,1,1", file.path(directory, "data.csv"))
  project <- file.path(directory, "project.mlxtran")
  writeLines(
    c(
      "<DATAFILE>",
      "file={path='data.csv'}",
      "[CONTENT]",
      "id = {use=identifier}",
      "time = {use=time}",
      "dv = {use=observation, yname={'1', '2'}, type={continuous, continuous}}",
      "dvid = {use=observationtype}",
      "<FIT>",
      "exportpath = 'RESULTS'"
    ),
    project
  )
  expect_identical(parse_mlxtran(project)$endpoint$code, "1")
})

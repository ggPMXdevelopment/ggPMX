if (helper_skip()) {

  context("Test pmx_read_nm_files")

  dir <-
    file.path(system.file(package = "ggPMX"), "testdata", "extdata")

  #-------------pmx_read_nm_files START ------------------------------------------
  test_that("pmx_read_nm_files: params: runno, ext, dir;
          result: identical class and structure", {
            ext_file <-
              pmx_read_nm_files(runno = "001", ext = ".ext", dir = dir)

            expect_true(inherits(ext_file, "tbl_df"))
            expect_identical(ext_file$name[1], "run001.ext")
            expect_true(all(
              names(ext_file) %in% c(
                "name",
                "extension",
                "problem",
                "subprob",
                "method",
                "data",
                "modified"
              )
            ))
          })

  test_that("pmx_read_nm_files: params: no; result: error", {
    expect_error(pmx_read_nm_files())
  })
  #-------------pmx_read_nm_files END --------------------------------------------

  #-------------pmx_parse_nm_files START -----------------------------------------
  test_that("pmx_parse_nm_files: params: raw data; result: TRUE", {
    full_path <-
      file.path(
        system.file(package = "ggPMX"),
        "testdata",
        "extdata",
        "run001.ext"
      )
    out <- full_path %>%
      dplyr::tibble(path = ., name = basename(.)) %>%
      dplyr::filter(file.exists(.$path)) %>%
      dplyr::mutate(
        grouping = 1:dplyr::n(),
        raw = purrr::map(.$path, .f = readr::read_lines)
      ) %>%
      dplyr::group_by_at(.vars = "grouping") %>%
      tidyr::nest() %>%
      dplyr::ungroup()

    df <- pmx_parse_nm_files(out$data[[1]])
    expect_true(inherits(df, "tbl_df"))
    expect_true(all(names(df) %in% c("problem", "subprob", "method", "data")))
  })
  #-------------pmx_parse_nm_files END -------------------------------------------

  #-------------pmx_raw_to_tibble START ------------------------------------------
  test_that("pmx_raw_to_tibble: params: x, sep, file;
          result: identical class and structure", {
            df <- data.frame(
              raw = c(
                "ITERATION, THETA1, THETA2, THETA3",
                "0, 2.53535E+01, 1.46525E+00, 7.45219E+00"
              ),
              header = c(TRUE, FALSE)
            )

            r <- pmx_raw_to_tibble(x = df, sep = ", ", file = "")
            expect_true(inherits(r, "data.frame"))
            expect_true(nrow(r) == 1)
            expect_true(all(names(r) %in% c("ITERATION", "THETA1", "THETA2", "THETA3")))
          })
  #-------------pmx_raw_to_tibble END --------------------------------------------
}

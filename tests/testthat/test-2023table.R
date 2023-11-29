if (helper_skip()) {
  test_that("Monolix 2023 tables read in correctly (Issue #369)", {
    skip_if_not(file.exists(test_path("warfarin_PD_project.zip")))
    .path <- normalizePath(test_path("warfarin_PD_project.zip"))

    withr::with_tempdir({

      unzip(.path)

      ctr <- pmx_mlxtran("warfarin_PD_project.mlxtran")

      p_ctr <- ctr %>% param_table(return_table = TRUE)

      Names <- c("PARAM", "VALUE", "SE", "RSE")

      expect_equal(names(p_ctr), Names)

    })
  })
}

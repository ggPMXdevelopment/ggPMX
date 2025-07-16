if (helper_skip()) {

  context("Test pmx_nm")

  test_that("read_extfile: params: npde, iwres, ipred;
          result: can read NONMEM-Output", {
            r <- read_extfile(
              project = NULL,
              file = "run001.ext",
              path = file.path(system.file(package = "ggPMX"), "testdata", "extdata", "run001.ext"),
              read_fun = "data.table",
              quiet = FALSE
            )

            expect_true(inherits(r, "list"))

          })

  test_that("read_extfile: params: project, path is NULL, file doesn't exist;
          result: error", {

            expect_error(read_extfile(
              run = "run001",
              project = file.path(system.file(package = "ggPMX"), "testdata", "extdata"),
              path = NULL,
              read_fun = "data.table",
              quiet = FALSE
            ))

          })

  test_that("read_extfile: params: project is NULL, read_fun  is read.table;
          result: error", {

            expect_error(read_extfile(
              project = NULL,
              file = "run001.ext",
              path = file.path(system.file(package = "ggPMX"), "testdata", "extdata", "run001.grd"),
              read_fun = "read.table",
              quiet = FALSES
            ))

          })

  test_that("pmx_nm: params: npde, iwres, ipred;
          result: can read NONMEM-Output", {

            ctr <- pmx_nm(
              directory = file.path(system.file(package = "ggPMX"), "testdata", "extdata"),
              npde = "TAD", iwres = "IWRES", ipred = "IPRED", runno = "001"
            )
            expect_true(inherits(ctr, "pmxClass"))

          })

  test_that("pmx_nm: params: runno is NULL, file is run001.coi;
          result: error", {

            nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
            expect_error(pmx_nm(directory = nonmem_dir, runno = NULL, file = "run001.coi"))

          })

  test_that("pmx_nm: params: simfile;
          result: can read NONMEM-Output", {

            nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
            ctr <- pmx_nm(directory = nonmem_dir, runno = "001", simfile = "custom_sim.lst")
            expect_true(inherits(ctr, "pmxClass"))

          })

  test_that("pmx_nm: params: obs is T;
          result: warning", {

            nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
            expect_warning(pmx_nm(directory = nonmem_dir, runno = "001", obs = TRUE))

          })

  test_that("pmx_nm: params: Endpoint value does not correspond to dvid values;
          result: warning", {

            nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
            expect_warning(pmx_nm(directory = nonmem_dir, runno = "001", endpoint = 12))

          })

  test_that("pmx_nm: params: Endpoint value corresponds to dvid values;
          result: error", {

            nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
            expect_fn <- if (packageVersion("GGally") <= "2.2.1") expect_error else expect_warning
            expect_fn(pmx_nm(directory = nonmem_dir, runno = "001", endpoint = 2.75, dvid = "TAD"))

          })

  test_that("pmx_nm: params: settings is not pmxSettingsClass object;
          result: can read NONMEM-Output", {

            nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
            ctr <- pmx_nm(directory = nonmem_dir, runno = "001", settings = list(use.titles = TRUE))

          })

  test_that("pmx_nm: params: file is NULL, runno is NULL;
          result: error", {

            expect_error(pmx_nm(file = NULL, runno = NULL))

          })

  test_that("pmx_nm: params: directory, file, simfile;
          result: can read NONMEM-Output", {
            skip_on_cran()
            file_to_check <- file.path(system.file(package = "ggPMX"), "testdata", "extdata", "for_testing", "predictions.csv")
            skip_if_not(file.exists(file_to_check))
            
            nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
            ctr <- pmx_nm(directory = nonmem_dir, runno = "001")
            # just to test alternative loadings
            ctr_lst <- pmx_nm(directory = nonmem_dir, file = "custom_name.lst", simfile = "custom_sim.lst")

            expect_true(inherits(ctr$settings, "pmxSettingsClass"))

            expect_setequal(
              plot_names(ctr),
              c(
                "abs_iwres_ipred", "abs_iwres_time", "iwres_ipred", "iwres_time", "iwres_dens",
                "iwres_qq", "dv_pred", "dv_ipred", "individual",
                "eta_hist", "eta_box", "eta_matrix", "eta_cats",
                "eta_conts", "eta_qq", "pmx_vpc"
              )
            )
            # check if controllers are the same #1
            expect_identical(
              names(ctr$input), names(ctr_lst$input)
            )

            # check if controllers are the same #2
            expect_identical(
              sort(ctr %>% plot_names()), sort(ctr_lst %>% plot_names())
            )


            # Check if ctr is made
            expect_identical(
              is.null(ctr), FALSE
            )

            # Check if header is correctly extracted and named from input
            expect_identical(
              names(ctr$input), c(
                "ID", "SEX", "MED1", "MED2", "DOSE", "AMT", "SS", "II", "TIME", "TAD", "IPRED",
                "CWRES", "CPRED", "IWRES", "EVID", "A1", "A2", "DV", "PRED", "RES", "WRES", "CLCR",
                "AGE", "WT", "KA", "CL", "V", "ALAG1", "ETA1", "ETA2", "ETA3", "isobserv"
              )
            )

            # Check if covariates were extracted correctly
            vec <- c(ctr$cats, ctr$conts)

            expect_identical(
              vec, c("SEX", "MED1", "MED2", "CLCR", "AGE", "WT")
            )

            # Check if data was extracted correctly
            csv_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata", "for_testing")

            input_file <- file.path(csv_dir, "predictions.csv")
            dat <- read.csv(input_file)

            expect_equal(
              nrow(ctr %>% get_data("predictions")), nrow(dat)
            )

            # Check if simulation data was extracted correctly
            simput_file <- file.path(csv_dir, "sim.csv")
            sim_dat <- read.csv(simput_file)

            expect_equal(
              nrow(ctr %>% get_data("sim")), nrow(sim_dat)
            )

            # check alternative import with and without runnumber
            # would normally cause many messages: No data eta provided for plot eta_qq etc.
            # suppressed for test via capture.output()
            ignore <- capture.output({
              ctr_norunno <- pmx_nm(directory = nonmem_dir, file = "run001.lst")
              ctr_man <- pmx_nm(directory = nonmem_dir, table_names = c("sdtab"), runno = "002")
              ctr_man_norunno <- pmx_nm(directory = nonmem_dir, table_names = c("sdtab002"))
            })

            expect_identical(
              ctr_man_norunno$settings,
              structure(
                list(
                  is.draft = TRUE, use.abbrev = TRUE, color.scales = NULL, use.labels = FALSE,
                  cats.labels = NULL, use.titles = FALSE, effects = NULL
                ),
                class = c("pmxSettingsClass")
              )
            )

            expect_setequal(
              plot_names(ctr_man_norunno),
              c(
                "abs_iwres_ipred", "abs_iwres_time", "iwres_ipred", "iwres_time", "iwres_dens", "iwres_qq",
                "dv_pred", "dv_ipred", "individual"
              )
            )

            expect_identical(
              names(ctr_man$input), c(
                "ID", "DOSE", "AMT", "SS", "II", "TIME", "TAD", "IPRED", "CWRES", "CPRED",
                "IWRES", "EVID", "A1", "A2", "DV", "PRED", "RES", "WRES", "isobserv"
              )
            )

            expect_identical(
              names(ctr_man$input), names(ctr_man_norunno$input)
            )

          })
}

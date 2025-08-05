if (helper_skip()) {

  context("Test param_table() with theophylline controller")
  ctr <- theophylline()

  test_that("can make param_table()", {
    # Creating "pmxClass" controllers:
    ctr <- theophylline()

    nonmem_dir <- file.path(system.file(package = "ggPMX"), "testdata", "extdata")
    ctr_nm <- pmx_nm(directory = nonmem_dir, runno = "001")

    ctr_nm <- pmx_nm(
      directory = file.path(system.file(package = "ggPMX"), "testdata", "extdata"),
      runno = "001"
    )

    # Creating kable outputs for testing:
    p_ctr <- param_table(ctr, digits = 2, scientific = FALSE)
    p_ctr_nm <- param_table(ctr_nm, digits = 2, scientific = FALSE)
    p_ctr_sci <- param_table(ctr, digits = 2, scientific = TRUE)

    # Check headers
    expect_true(
      "|Parameter                                        |Value  |RSE   |Shrinkage |" %in% trimws(p_ctr)
    )

    expect_true(
      "|Parameter  |Value  |RSE    |Shrinkage     |" %in% trimws(p_ctr_nm)
    )

    # check a random row (here 5) of param_table
    expect_true(
      "|Cl                                               |0.31   |8%    |          |" %in% trimws(p_ctr),
      )

    expect_true(
      "|THETA1     |26     |3%     |              |" %in% trimws(p_ctr_nm),
      )

    # Check class:
    expect_s3_class(p_ctr, "knitr_kable")
    expect_s3_class(p_ctr_nm, "knitr_kable")

    # Check output lengths:
    expect_length(p_ctr, 23L)
    expect_length(p_ctr_nm, 23L)
    expect_length(p_ctr_sci, 23L)

    # Check scientific notation:
    p_ctr_sci <- param_table(ctr, digits = 2, scientific = TRUE)
    expect_true(any(grepl("\\de\\+\\d", p_ctr_sci)))
    expect_true(any(grepl("\\de\\-\\d", p_ctr_sci)))
  })

  test_that("param_table: params return: equal tables, identical names", {
    p_ctr <- ctr %>% param_table(return_table = TRUE)
    pop_pars <- ctr %>% get_data("estimates")
    Names <- c("PARAM", "VALUE", "SE", "RSE", "PVALUE")
    expect_equal(p_ctr, pop_pars)
    expect_identical(names(p_ctr), Names)
  })

  test_that("param_table: params return: identical sys in config", {
    p_t <- ctr %>% param_table(return_table = TRUE)
    expect_identical(ctr$config$sys, "mlx")
  })

  test_that("param_table: params: fun return: message `var` was used for shrinkage calculation", {
    p_t <- ctr %>% param_table(fun = "var")
    expect_message(ctr %>% param_table())
  })

  test_that("param_table: params NULL return:  identical inherits", {
    expect_s3_class(ctr %>% param_table, "knitr_kable")
  })

  #------------------- param_table with nlmixr start -----------------------------
  context("Test param_table() with nlmixr controller")
  if (requireNamespace("nlmixr2est", quietly=TRUE)) {
    test_that("param_table: params return: kable", {
      skip_on_os("windows")
      one.compartment <- function() {
        ini({
          tka <- 0.45 # Log Ka
          tcl <- 1 # Log Cl
          tv <- 3.45 # Log V
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          d / dt(depot) <- -ka * depot
          d / dt(center) <- ka * depot - cl / v * center
          cp <- center / v
          cp ~ add(add.sd)
        })
      }
      fit <- nlmixr2est::nlmixr(one.compartment, nlmixr2data::theo_sd, "saem",
                             control = list(print = 0)
                             )
      ctr <- pmx_nlmixr(fit, conts = c("cl", "v"))
      expect_s3_class(param_table(ctr), "knitr_kable")
    })
  }
  #------------------- param_table with nlmixr start -----------------------------
}

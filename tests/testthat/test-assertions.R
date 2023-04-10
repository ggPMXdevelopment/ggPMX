context("Test assertions")

#------------------- is_string start ------------------------------------------

test_that("is_string: character variable is string", {
  str <- "hello"
  expect_true(is_string(str))
})

test_that("is_string: numeric variable is not string", {
  str <- 123
  expect_false(is_string(str))
})

test_that("is_string: NA variable is not string", {
  str <- NA
  expect_false(is_string(str))
})

test_that("is_string: vector variable is not string", {
  str <- c("A", "B")
  expect_false(is_string(str))
})
#------------------- is_string end---------------------------------------------

#------------------- is_character_or_null start--------------------------------

test_that("is_character_or_null: character variable is character", {
  str <- "hello"
  expect_true(is_character_or_null(str))
})

test_that("is_character_or_null: numeric variable is not character or null", {
  str <- 123
  expect_false(is_character_or_null(str))
})

test_that("is_character_or_null: NULL variable is null", {
  str <- NULL
  expect_true(is_character_or_null(str))
})

test_that("is_character_or_null: vector of characters is character", {
  str <- c("A", "B")
  expect_true(is_character_or_null(str))
})
#------------------- is_character_or_null end----------------------------------

#------------------- is_string_or_null start-----------------------------------

test_that("is_string_or_null: character variable is string", {
  str <- "hello"
  expect_true(is_string_or_null(str))
})

test_that("is_string_or_null: numeric variable is not string", {
  str <- 123
  expect_false(is_string_or_null(str))
})

test_that("is_string_or_null: NULL variable is null", {
  str <- NULL
  expect_true(is_string_or_null(str))
})

test_that("is_string_or_null: vector of characters is not string", {
  str <- c("A", "B")
  expect_false(is_string_or_null(str))
})
#------------------- is_string_or_null end-------------------------------------

#------------------- is_string_or_expression start-----------------------------

test_that("is_string_or_expression: character variable is string or expr", {
  str <- "hello"
  expect_true(is_string_or_expression(str))
})

test_that("is_string_or_expression: numeric variable is not string or expr", {
  str <- 123
  expect_false(is_string_or_expression(str))
})

test_that("is_string_or_expression: expression variable is expression", {
  str <- expression(1 + 0:9)
  expect_true(is_string_or_expression(str))
})

test_that("is_string_or_expression: expression with function is expr", {
  str <- expression(1 + sin(pi))
  expect_true(is_string_or_expression(str))
})

test_that("is_string_or_expression:
          vector of characters is not string or expr", {
  str <- c("A", "B")
  expect_false(is_string_or_expression(str))
})
#------------------- is_string_or_expression end-------------------------------

#------------------- is_string_or_expression_or_null start---------------------

test_that(
  "is_string_or_expression_or_null:
  character variable is string or expr or null",
  {
    str <- "hello"
    expect_true(is_string_or_expression_or_null(str))
  }
)

test_that(
  "is_string_or_expression_or_null:
  numeric variable is not string or expr or null",
  {
    str <- 123
    expect_false(is_string_or_expression_or_null(str))
  }
)

test_that("is_string_or_expression_or_null: NULL variable is null", {
  str <- NULL
  expect_true(is_string_or_expression_or_null(str))
})

test_that("is_string_or_expression_or_null: expression with function is expr", {
  str <- expression(1 + sin(pi))
  expect_true(is_string_or_expression_or_null(str))
})

test_that("is_string_or_expression_or_null: expression variable is expr", {
  str <- expression(1 + 0:9)
  expect_true(is_string_or_expression_or_null(str))
})
#------------------- is_string_or_expression_or_null end-----------------------

#------------------- is_string_or_formula_or_null start------------------------

test_that("is_string_or_formula_or_null:
          character variable is string, formula or null", {
  str <- "hello"
  expect_true(is_string_or_formula_or_null(str))
})

test_that("is_string_or_formula_or_null:
          numeric variable is not string or formula or null", {
  str <- 123
  expect_false(is_string_or_formula_or_null(str))
})

test_that("is_string_or_formula_or_null: formula variable is formula", {
  x <- 10
  str <- y ~ x
  expect_true(is_string_or_formula_or_null(str))
})

test_that("is_string_or_formula_or_null: NULL variable is null", {
  str <- NULL
  expect_true(is_string_or_formula_or_null(str))
})
#------------------- is_string_or_formula_or_null end--------------------------

#------------------- is_null_or_named_vector start-----------------------------

test_that("is_null_or_named_vector: character named vector is named vector", {
  vec <- c("one", "two", "three")
  names(vec) <- c("A", "B", "C")
  expect_true(is_null_or_named_vector(vec))
})

test_that("is_null_or_named_vector: numeric variable is not named vector", {
  vec <- 123
  expect_false(is_null_or_named_vector(vec))
})

test_that("is_null_or_named_vector: NULL variable is null", {
  vec <- NULL
  expect_true(is_null_or_named_vector(vec))
})

test_that("is_null_or_named_vector: string variable is not named vector", {
  vec <- "hello"
  expect_false(is_null_or_named_vector(vec))
})

test_that("is_null_or_named_vector:
          character unnamed vector is not named vector", {
  vec <- c("one", "two", "three")
  expect_false(is_null_or_named_vector(vec))
})
#------------------- is_null_or_named_vector start-----------------------------

#------------------- is_pmx_gpar start-----------------------------------------

test_that("is_pmx_gpar: pmx gpar variable is pmx gpar", {
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_true(is_pmx_gpar(gpars))
})

test_that("is_pmx_gpar: numeric variable is not pmx gpar", {
  gpars <- 123
  expect_false(is_pmx_gpar(gpars))
})

test_that("is_pmx_gpar: NULL variable is not pmx gpar", {
  gpars <- NULL
  expect_false(is_pmx_gpar(gpars))
})

test_that("is_pmx_gpar: list is not pmx gpar", {
  gpars <- list("A", "B")
  expect_false(is_pmx_gpar(gpars))
})
#------------------- is_pmx_gpar end-------------------------------------------

#------------------- is_configs start------------------------------------------

test_that("is_configs: config variable is configs", {
  config <- pmx_get_configs()
  expect_true(is_configs(config))
})

test_that("is_configs: numeric variable is not configs", {
  config <- 123
  expect_false(is_configs(config))
})

test_that("is_configs: NULL variable is not configs", {
  config <- NULL
  expect_false(is_configs(config))
})

test_that("is_configs: character variable is not configs", {
  config <- "hello"
  expect_false(is_configs(config))
})
#------------------- is_configs end--------------------------------------------

#------------------- is_pmxconfig start----------------------------------------

test_that("is_pmxconfig: pmxConfig variable is pmxConfig", {
  sys <- "mlx"
  input_dir <-
    file.path(system.file(package = "ggPMX"), "templates", sys)
  plot_dir <- file.path(system.file(package = "ggPMX"), "init")
  ifile <- file.path(input_dir, sprintf("%s.ipmx", "standing"))
  pfile <- file.path(plot_dir, sprintf("%s.ppmx", "standing"))
  config <- load_config_files(ifile, pfile, sys)
  expect_true(is_pmxconfig(config))
})

test_that("is_pmxconfig: numeric variable is not pmxConfig", {
  config <- 123
  expect_false(is_pmxconfig(config))
})

test_that("is_pmxconfig: NULL variable is not pmxConfig", {
  config <- NULL
  expect_false(is_pmxconfig(config))
})

test_that("is_pmxconfig: character variable is not pmxConfig", {
  config <- "hello"
  expect_false(is_pmxconfig(config))
})
#------------------- is_pmxconfig end------------------------------------------

#------------------- is_pmxclass start-----------------------------------------

test_that("is_pmxclass: pmxclass variable is pmxclass", {
  ctr <- theophylline()
  expect_true(is_pmxclass(ctr))
})

test_that("is_pmxclass: numeric variable is not pmxclass", {
  ctr <- 123
  expect_false(is_pmxclass(str))
})

test_that("is_pmxclass: NA variable is not pmxclass", {
  ctr <- NA
  expect_false(is_pmxclass(ctr))
})

test_that("is_pmxclass: NULL variable is not pmxclass", {
  ctr <- NULL
  expect_false(is_pmxclass(ctr))
})
#------------------- is_pmxclass end-------------------------------------------

#------------------- is_ggplot start-------------------------------------------

test_that("is_ggplot: ggplot variable is ggplot", {
  ctr <- theophylline()
  p <- pmx_plot_abs_iwres_ipred(ctr)
  expect_true(is_ggplot(p))
})

test_that("is_ggplot: numeric variable is not ggplot", {
  ctr <- 123
  expect_false(is_ggplot(ctr))
})

test_that("is_ggplot: NA variable is not ggplot", {
  ctr <- NA
  expect_false(is_ggplot(ctr))
})

test_that("is_ggplot: NULL variable is not ggplot", {
  ctr <- NULL
  expect_false(is_ggplot(ctr))
})
#------------------- is_ggplot end---------------------------------------------

#------------------- is_logical start------------------------------------------
test_that("is_logical: logical variable is logical", {
  v <- TRUE
  expect_true(is_logical(v))
})

test_that("is_logical: numeric variable is not logical", {
  v <- 123
  expect_false(is_logical(v))
})

test_that("is_logical: character variable is not logical", {
  v <- "hello"
  expect_false(is_logical(v))
})

test_that("is_logical: NULL variable is not logical", {
  v <- NULL
  expect_false(is_logical(v))
})
#------------------- is_logical end--------------------------------------------

#------------------- is_list start---------------------------------------------

test_that("is_list: list variable is list", {
  v <- list("A", "B", "C")
  expect_true(is_list(v))
})

test_that("is_list: numeric variable is not list", {
  v <- 123
  expect_false(is_list(v))
})

test_that("is_list: logical variable is not list", {
  v <- TRUE
  expect_false(is_list(v))
})

test_that("is_list: NULL variable is not list", {
  v <- NULL
  expect_false(is_list(v))
})
#------------------- is_list end-----------------------------------------------

#------------------- is_list_or_null start-------------------------------------

test_that("is_list_or_null: list variable is list or null", {
  v <- list("A", "B", "C")
  expect_true(is_list_or_null(v))
})

test_that("is_list_or_null: numeric variable is not list or null", {
  v <- 123
  expect_false(is_list_or_null(v))
})

test_that("is_list_or_null: NULL variable is null", {
  v <- NULL
  expect_true(is_list_or_null(v))
})

test_that("is_list_or_null: NA variable is not list or null", {
  v <- NA
  expect_false(is_list_or_null(v))
})
#------------------- is_list_or_null end---------------------------------------

#------------------- is_integer_or_null start----------------------------------

test_that("is_integer_or_null: integer variable is integer or null", {
  v <- 123L
  expect_true(is_integer_or_null(v))
})

test_that("is_integer_or_null: list variable is not integer or null", {
  v <- list("A", "B", "C")
  expect_false(is_integer_or_null(v))
})

test_that("is_integer_or_null: float variable is not integer", {
  v <- 12.3
  expect_false(is_integer_or_null(v))
})

test_that("is_integer_or_null: NULL variable is null", {
  v <- NULL
  expect_true(is_integer_or_null(v))
})
#------------------- is_integer_or_null end------------------------------------

#------------------- is_language_or_string start-------------------------------

test_that("is_language_or_string: variable is language", {
  v <- expression(x^2 - 2 * x + 1)
  expect_true(is_language_or_string(v))
})

test_that("is_language_or_string: variable is not language or string", {
  v <- list("A", "B", "C")
  expect_false(is_language_or_string(v))
})

test_that("is_language_or_string: variable is string", {
  v <- "hello"
  expect_true(is_language_or_string(v))
})

test_that("is_language_or_string: variable is language", {
  v <- call("sin", pi)
  expect_true(is_language_or_string(v))
})
#------------------- is_language_or_string end---------------------------------

#------------------- is_valid_plot_name start----------------------------------

test_that("is_valid_plot_name: variable is valid plot name", {
  ctr <- theophylline()
  plots <- ctr %>% plot_names()
  x <- "abs_iwres_ipred"
  expect_true(is_valid_plot_name(x, plots))
})

test_that("is_valid_plot_name: variable is not valid plot name", {
  ctr <- theophylline()
  plots <- ctr %>% plot_names()
  x <- "test_iwres_ipred"
  expect_false(is_valid_plot_name(x, plots))
})

test_that("is_valid_plot_name: NA is not valid plot name", {
  ctr <- theophylline()
  plots <- ctr %>% plot_names()
  x <- NA
  expect_false(is_valid_plot_name(x, plots))
})
#------------------- is_valid_plot_name end------------------------------------

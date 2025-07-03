is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_character_or_null <- function(x) {
  (is.character(x) && all(!is.na(x))) || is.null(x)
}


assertthat::on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string")
}

assertthat::on_failure(is_character_or_null) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid character vector")
}

is_string_or_null <- function(x) {
  is_string(x) || is.null(x)
}

is_string_or_expression <- function(x) {
  is_string(x) || is.expression(x)
}

is_string_or_expression_or_null <- function(x) {
  is_string(x) || is.expression(x) || is.null(x)
}


is_string_or_formula_or_null <- function(x) {
  is_string(x) || is.formula(x) || is.null(x)
}

#In contrast to is_string_or_formula_or_null returns TRUE for c('A', 'B')
#Also c('A', NA) ~ TRUE, NA ~ TRUE
is_character_or_formula_or_null_or_na <- function(x) {
  is.character(x) || is.formula(x) || is.null(x) || is.na(x)
}

is_null_or_named_vector <- function(v) {
  is.null(v) || is.vector(v) & !is.null(names(v)) & all(!is.na(names(v)))
}

assertthat::on_failure(is_null_or_named_vector) <- function(call, env) {
  paste0(deparse(call$x), " is not a named vector or NULL")
}
assertthat::on_failure(is_string_or_formula_or_null) <- function(call, env) {
  paste0(deparse(call$x), " is not a formula or NULL")
}


assertthat::on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " is not a string or NULL")
}

assertthat::on_failure(is_string_or_expression_or_null) <- function(call, env) {
  paste0(deparse(call$x), " is not a string or expression or NULL")
}


assertthat::on_failure(is_string_or_expression) <- function(call, env) {
  paste0(deparse(call$x), " is not a string or expression")
}

is_pmx_gpar <- function(x) {
  is.pmx_gpar(x)
}

assertthat::on_failure(is_pmx_gpar) <- function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'pmx_gpar'.")
}

is_configs <- function(x) {
  inherits(x, "configs")
}

assertthat::on_failure(is_configs) <- function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'configs'.")
}

is_pmxconfig <- function(x) {
  inherits(x, "pmxConfig")
}

assertthat::on_failure(is_pmxconfig) <- function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'pmxConfig'.")
}

is_pmxclass <- function(x) {
  inherits(x, "pmxClass")
}

assertthat::on_failure(is_pmxclass) <- function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'pmxClass'.")
}

is_ggplot <- function(x) {
  if ("is_ggplot" %in% getNamespaceExports("ggplot2")) {
    ggplot2::is_ggplot(x)
  } else {
    ggplot2::is.ggplot(x)
  }
}

assertthat::on_failure(is_ggplot) <- function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'ggplot'.")
}

is_logical <- function(x) {
  is.logical(x)
}

assertthat::on_failure(is_logical) <- function(call, env) {
  paste0(deparse(call$x), " should be an logical vector.")
}

is_list <- function(x) {
  is.list(x)
}

assertthat::on_failure(is_list) <- function(call, env) {
  paste0(deparse(call$x), " is not a list")
}

is_list_or_null <- function(x) {
  is.list(x) || is.null(x)
}

assertthat::on_failure(is_list_or_null) <- function(call, env) {
  paste0(deparse(call$x), " is not a list or NULL")
}

is_integer_or_null <- function(x) {
  is.integer(x) || is.null(x)
}

assertthat::on_failure(is_integer_or_null) <- function(call, env) {
  paste0(deparse(call$x), " should be an integer value or NULL.")
}

is_language_or_string <- function(x) {
  is.language(x) || is_string(x)
}

assertthat::on_failure(is_language_or_string) <- function(call, env) {
  paste0(deparse(call$x), " should be an expression or a string")
}

is_valid_plot_name <- function(x, plots) {
  x %in% plots
}

assertthat::on_failure(is_valid_plot_name) <- function(call, env) {
  sprintf(
    "%s is not a valid plot name\nvalid plots are: %s",
    env$nplot,
    paste(env$ctr %>% plot_names(), collapse = "--")
  )
}

assertthat::on_failure(is_valid_plot_name) <- function(call, env) {
  sprintf(
    "%s is not a valid plot name\nvalid plots are: %s",
    env$nplot,
    paste(env$ctr %>% plot_names(), collapse = "--")
  )
}

is_none_empty_queue <- function(x) {
  length(x$report_queue) > 0
}

assertthat::on_failure(is_none_empty_queue) <- function(call, env) {
  sprintf("Chunk has plots that were not registered within ggPMX. Footnotes may be wrong.")
}

is_empty_queue <- function(x) {
  length(x$report_queue) == 0
}

assertthat::on_failure(is_empty_queue) <- function(call, env) {
  sprintf("Plot(s) were registered within ggPMX but were not rendered. Footnotes may be wrong.")
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

assertthat::on_failure(is_string) <-  function(call, env) {
  paste0(deparse(call$x), " is not a string")
}

is_pmx_gpar <- function(x) {
  is.pmx_gpar(x)
}

assertthat::on_failure(is_pmx_gpar) <-  function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'pmx_gpar'.")
}

is_configs <- function(x) {
  inherits(x, "configs")
}

assertthat::on_failure(is_configs) <-  function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'configs'.")
}

is_pmxconfig <- function(x) {
  inherits(x, "pmxConfig")
}

assertthat::on_failure(is_pmxconfig) <-  function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'pmxConfig'.")
}

is_ggplot <- function(x){
  ggplot2::is.ggplot(x)
}

assertthat::on_failure(is_ggplot) <-  function(call, env) {
  paste0(deparse(call$x), " is not an object of class 'ggplot'.")
}

is_logical <- function(x){
  is.logical(x)
}

assertthat::on_failure(is_logical) <- function(call, env) {
  paste0(deparse(call$x), " should be an logical vector.")
}

is_list <- function(x){
  is.list(x)
}

assertthat::on_failure(is_list) <- function(call, env){
  paste0(deparse(call$x), " is not a list")
}

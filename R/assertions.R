is_pmx_gpar <- function(x) {
  is.pmx_gpar(x)
}

assertthat::on_failure(is_pmx_gpar) <-  function(call, env) {
  paste0(deparse(call$x), " is not a pmx_gpar object")
}

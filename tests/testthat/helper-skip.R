helper_skip <- function() {
  ret <- isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
  #ret
  FALSE
}

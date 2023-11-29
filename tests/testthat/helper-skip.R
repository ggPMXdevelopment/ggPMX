helper_skip <- function() {
  interactive() || isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}

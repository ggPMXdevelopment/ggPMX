helper_skip <- function() {
  TRUE
}

helper_run_slow_tests <- function() {
  identical(tolower(Sys.getenv("GGPMX_RUN_SLOW_TESTS", "false")), "true")
}

skip_slow_tests <- function() {
  testthat::skip_if_not(
    helper_run_slow_tests(),
    "Set GGPMX_RUN_SLOW_TESTS=true to run extended integration tests"
  )
}

helper_run_windows_integration <- function(os = .Platform$OS.type) {
  os != "windows" || helper_run_slow_tests()
}

skip_windows_integration <- function() {
  testthat::skip_if_not(
    helper_run_windows_integration(),
    "Extended Windows integration: set GGPMX_RUN_SLOW_TESTS=true"
  )
}

local({
  library_path <- Sys.getenv("GGPMX_LIBRARY")
  if (nzchar(library_path)) .libPaths(c(library_path, .libPaths()))
  harness <- normalizePath(Sys.getenv("GGPMX_HARNESS", "tools/monolix"), mustWork = TRUE)
  output <- Sys.getenv("GGPMX_MONOLIX_OUTPUT")
  demos <- Sys.getenv("GGPMX_MONOLIX_DEMOS")
  mode <- Sys.getenv("GGPMX_MONOLIX_MODE", "regenerate")
  if (!mode %in% c("exported", "regenerate")) stop("Mode must be exported or regenerate")
  if (!nzchar(output) || !dir.exists(demos)) stop("Set GGPMX_MONOLIX_OUTPUT and GGPMX_MONOLIX_DEMOS")
  if (file.exists(output)) stop("Output path already exists; choose a fresh directory")
  dir.create(output, recursive = TRUE)
  output <- normalizePath(output, mustWork = TRUE)
  Sys.setenv(GGPMX_HARNESS = harness, GGPMX_MONOLIX_OUTPUT = output, GGPMX_MONOLIX_MODE = mode)
  writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
  if (mode == "regenerate") {
    library(lixoftConnectors)
    install <- Sys.getenv("GGPMX_MONOLIX_HOME", Sys.getenv("EBROOTMONOLIX"))
    if (!dir.exists(install)) stop("Set GGPMX_MONOLIX_HOME to the Monolix installation")
    lixoftConnectors::initializeLixoftConnectors(software = "monolix", path = install, force = TRUE)
    state <- lixoftConnectors::getLixoftConnectorsState()
    if (is.null(state$version)) stop("Monolix initialization failed")
    writeLines(capture.output(str(state)), file.path(output, "monolix-info.txt"))
  }
  reporters <- testthat::MultiReporter$new(list(
    testthat::SummaryReporter$new(),
    testthat::JunitReporter$new(file = file.path(output, "junit.xml"))
  ))
  testthat::test_file(file.path(harness, "test-demos.R"), reporter = reporters, stop_on_failure = TRUE)
})

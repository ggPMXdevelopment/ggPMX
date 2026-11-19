library(testthat)
library(ggPMX)
library(data.table)
setDTthreads(1)
source(file.path("support", "timing-reporter.R"))
test_check(
  "ggPMX",
  reporter = MultiReporter$new(list(
    file_timing_reporter("test-file-timings.csv"),
    CheckReporter$new()
  ))
)

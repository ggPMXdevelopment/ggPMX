file_timing_reporter <- function(output) {
  R6::R6Class(
    inherit = testthat::Reporter,
    public = list(
      timings = list(),
      filename = NULL,
      file_start = NULL,
      test_start = NULL,
      execution = 0,
      start_file = function(filename) {
        self$filename <- filename
        self$execution <- 0
        self$file_start <- proc.time()[["elapsed"]]
      },
      start_test = function(context, test) {
        self$test_start <- proc.time()[["elapsed"]]
      },
      end_test = function(context, test) {
        self$execution <- self$execution +
          proc.time()[["elapsed"]] -
          self$test_start
      },
      end_file = function() {
        total <- proc.time()[["elapsed"]] - self$file_start
        self$timings[[length(self$timings) + 1L]] <- data.frame(
          file = self$filename,
          total_seconds = total,
          outside_tests_seconds = total - self$execution,
          test_seconds = self$execution
        )
      },
      end_reporter = function() {
        timings <- do.call(rbind, self$timings)
        if (is.null(timings)) {
          return(invisible(NULL))
        }
        timings <- timings[order(timings$total_seconds, decreasing = TRUE), ]
        utils::write.csv(timings, output, row.names = FALSE)
        print(timings, row.names = FALSE)
      }
    )
  )$new()
}

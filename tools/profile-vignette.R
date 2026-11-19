args <- commandArgs(trailingOnly = TRUE)
output_dir <- normalizePath(
  if (length(args)) args[[1]] else "vignette-timing",
  mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

work_dir <- file.path(output_dir, "vignettes")
unlink(work_dir, recursive = TRUE)
dir.create(work_dir)
file.copy(
  list.files("vignettes", full.names = TRUE),
  work_dir,
  recursive = TRUE
)

if (!"package:ggPMX" %in% search()) {
  library(ggPMX)
}

chunk_timings <- list()
chunk_starts <- new.env(parent = emptyenv())
knitr::knit_hooks$set(profile = function(before, options, envir) {
  label <- options$label
  if (before) {
    chunk_starts[[label]] <- proc.time()[["elapsed"]]
  } else {
    chunk_timings[[length(chunk_timings) + 1L]] <<- data.frame(
      chunk = label,
      seconds = proc.time()[["elapsed"]] - chunk_starts[[label]]
    )
  }
  invisible(NULL)
})
knitr::opts_chunk$set(
  profile = TRUE,
  fig.path = file.path(work_dir, "figure", "")
)

time_stage <- function(stage, expr) {
  elapsed <- system.time(force(expr))[["elapsed"]]
  data.frame(stage = stage, seconds = elapsed)
}

input <- file.path(work_dir, "ggPMX-guide.Rmd")
markdown <- file.path(work_dir, "ggPMX-guide.md")
latex <- file.path(work_dir, "ggPMX-guide.tex")
old_wd <- setwd(work_dir)
on.exit(setwd(old_wd), add = TRUE)

stages <- rbind(
  time_stage("knitr chunks and markdown", {
    knitr::knit(input, markdown, quiet = TRUE, envir = new.env())
  }),
  time_stage("pandoc to LaTeX", {
    rmarkdown::render(
      markdown,
      output_format = rmarkdown::latex_document(
        fig_caption = TRUE,
        number_sections = TRUE,
        toc = TRUE,
        toc_depth = 3,
        extra_dependencies = c("graphicx", "grffile")
      ),
      output_file = basename(latex),
      quiet = TRUE
    )
  }),
  time_stage("LaTeX to PDF", tinytex::latexmk(latex, engine = "pdflatex"))
)

chunks <- do.call(rbind, chunk_timings)
chunks <- chunks[order(chunks$seconds, decreasing = TRUE), ]
utils::write.csv(
  stages,
  file.path(output_dir, "vignette-stage-timings.csv"),
  row.names = FALSE
)
utils::write.csv(
  chunks,
  file.path(output_dir, "vignette-chunk-timings.csv"),
  row.names = FALSE
)

print(stages, row.names = FALSE)
print(utils::head(chunks, 20), row.names = FALSE)

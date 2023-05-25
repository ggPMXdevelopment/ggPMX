#' Generates ggpmX report from a pre-defined template
#'
#' @param contr \code{pmxClass} controller
#' @param name \code{character} The report name
#' @param output \code{character} the result type, can be \cr
#' a standalone directory of plots or a report document as defined in the template \cr
#' (pdf, docx,..) ,or both
#' @param template \code{character} ggPMX predefined template or the
#' path to a custom rmarkdown template. \cr
#' Use \code{\link{pmx_report_template}} to get the list
#' of available templates

#' @param save_dir Output directory. A directory to write the results files to
#' @param plots_subdir Output folder name, ggpmx_GOF by default
#' @param footnote \code{logical}  TRUE to add a footnote to the generated plots. The default footnote is to add \cr
#' the path where the plot is saved.
#' @param edit \code{logical}  TRUE to edit the template immediately
#' @param format \code{character} The output document format. By default, a word report is generated. \cr
#'  User can specify one or more formats from c("word","pdf","html","all"). format "all" to generate all formats.
#' @param title \code{character} report title (optional)
#' @param ... extra parameters depending in the template used
#' @export
#' @importFrom rmarkdown draft render
#' @importFrom knitr opts_chunk knit_hooks
#' @details
#' \code{pmx_report} uses pre-defined template .Rmd to generate the report.
#' The idea is to pass the controller as a report argument using knitr \code{params} artifact.

#' @example inst/examples/pmx_report.R

pmx_report <-
  function(contr,
             name,
             save_dir,
             plots_subdir = "ggpmx_GOF",
             output = c("all", "plots", "report"),
             template = "standing",
             footnote = output == "all",
             edit = FALSE,
             format = NULL,
             title,
             ...) {

    assert_that(
      is_pmxclass(contr),
      is.character(name),
      length(name) == 1L,
      is.character(template),
      length(template) == 1L
    )
    
    output <- match.arg(output)
    if (missing(format) || is.null(format)) format <- "word"
    if (sum(format %in% c("word","pdf","html","all")) == 0) {
      stop("format must be one or more of the following formats:\nc(\"word\",\"pdf\",\"html\",\"all\")", call.=FALSE)
    }

    if (!"all" %in% format) {
      format <- sprintf("%s_document", format)
    } else {
      format <- "all"
    }
    on.exit({
      remove_temp_files(contr$save_dir)
      contr$footnote <- FALSE
    })
    if (missing(save_dir) || is.null(save_dir)) {
      stop(sprintf("please provide a valid save directory"))
    }
    if (!dir.exists(save_dir)) {
      stop(sprintf("please provide a valid save directory : %s", save_dir))
    }
    save_dir <- path.expand(save_dir)
    contr$save_dir <- tools::file_path_as_absolute(save_dir)

    contr$footnote <- footnote
    res <- pmx_draft(contr, name, template, edit = FALSE)
    standalone <- output %in% c("plots", "all")
    clean <- !standalone
    old_fig_process <- knitr::opts_chunk$get("fig.process")

    out_ <- file.path(contr$save_dir, plots_subdir)

    rm_dir(out_)


    dir.create(out_)

    pmx_fig_process_init(contr)

    opts_chunk$set(
      fig.process = function(old_name) {
        pmx_fig_process(
          ctr = contr,
          old_name = old_name,
          footnote = footnote,
          out_
        )
      }
    )

    envir <- new.env()
    envir$ctr <- contr
    params <- list(ctr = contr, ...)
    if (!missing(title)) {
      params$title <- title
    } else {
      params$title <- "ggPMX standing report"
    }
    suppressWarnings(render(
      res,
      params = params,
      envir = envir,
      output_format = format,
      output_dir = save_dir,
      clean = clean,
      quiet = TRUE
    ))

    knitr::opts_chunk$set(fig.process = old_fig_process)

    pmx_fig_process_wrapup(contr)

    plot_dir <- sprintf("%s_files", name)
    in_ <- file.path(contr$save_dir, plot_dir)
    rm_dir(in_)

    if (!clean) {
      ## create_ggpmx_gof(ctr$save_dir, name, plots_subdir)
      remove_reports(output, contr$save_dir)
    }
    if (output == "report") rm_dir(out_)
  }


pmx_fig_process <- function(ctr, old_name, footnote, out_) {
  pname <- if (footnote) {
    suffix <- tools::file_ext(old_name)
    sprintf("%s.%s", ctr$dequeue_plot(), suffix)
  } else {
    basename(old_name)
  }

  new_name <- file.path(out_, pname)

  if (length(new_name)) {
    file.copy(old_name, new_name, copy.mode=FALSE)
    return(new_name)
  }

  return(old_name)
}


pmx_draft <- function(ctr, name, template, edit) {
  template_file <- file.path(ctr[["save_dir"]], sprintf("%s.Rmd", name))

  if (length(template_file) > 0 && file.exists(template_file)) {
    file.remove(template_file)
  }

  style_file <- file.path(ctr[["save_dir"]], "header.tex")
  if (file.exists(style_file)) file.remove(style_file)

  if (grepl(".Rmd", template) && !file.exists(template)) {
    stop(sprintf("!Template %s DO NOT EXIST", template))
  }

  if (file.exists(template)) {
    ggPMX_dir <- system.file(package = "ggPMX")

    standing_file <- system.file(
      "rmarkdown",
      "templates",
      ifelse(
        ctr$config$hasNpd &
          dir.exists(file.path(ggPMX_dir, "rmarkdown", "templates", "npd")),
        "npd",
        "standing"
      ),
      package="ggPMX")

    # Defining template and skeleton temporary subdirs, creating if missing
    # tempdir includes random subdir, so it is new for each run
    temp_dir <- file.path(tempdir(), paste0(sample(letters, 9), collapse=""))

    template_dir <- file.path(temp_dir, "standing")
    if (!dir.exists(template_dir)) {
      invisible(dir.create(template_dir, recursive=TRUE))
    }

    skeleton_dir <- file.path(template_dir, "skeleton")
    if (!dir.exists(skeleton_dir)) {
      invisible(dir.create(skeleton_dir, recursive=TRUE))
    }

    # Copying template and skeleton files to subdirectories
    invisible({
      file.copy(
        from = file.path(standing_file, "template.yaml"),
        to = template_dir,
        overwrite = TRUE,
        recursive = TRUE
      )
      # Determine source of template file (variable may be a directory or a file)
      template_source <-
        if(grepl("\\.[A-z]{2,}$", template)) {
          template
        } else {
          file.path(template, "skeleton", "skeleton.Rmd")
        }

      file.copy(
        from = template_source,
        to = file.path(skeleton_dir, "skeleton.Rmd"),
        overwrite = TRUE
      )
    })

    res <- draft(
      template_file,
      template = template_dir,
      create_dir = FALSE,
      edit = edit
    )
    unlink(temp_dir)
  } else {
    res <- draft(
      template_file,
      template = template,
      package = "ggPMX",
      create_dir = FALSE,
      edit = edit
    )
  }
  res
}


remove_temp_files <-
  function(save_dir) {
    temp_files <-
      list.files(
        pattern = "[.]md$|[.]tex$",
        path = save_dir,
        full.names = TRUE
      )
    invisible(file.remove(temp_files))
  }


remove_reports <- function(output, save_dir) {
  if (output == "plots") {
    invisible(file.remove(
      list.files(pattern="(.pdf|.docx|Rmd)$", path=save_dir, full.names=TRUE
    )))
  }
}


create_ggpmx_gof <- function(save_dir, name, plots_subdir = "ggpmx_GOF") {
  plot_dir <- sprintf("%s_files", name)
  if (dir.exists(file.path(save_dir, plot_dir))) {
    out_ <- file.path(save_dir, plots_subdir)
    rm_dir(out_)
    dir.create(out_)
    in_ <- file.path(save_dir, plot_dir)
    plots_ <- list.files(in_, recursive = TRUE, full.names = TRUE)

    idx <- grepl("^indiv", basename(plots_))
    indiv <- plots_[idx]
    no_indiv <- plots_[!idx]
    if (length(no_indiv) > 0) {
      no_indiv_dest <- gsub("[-]\\d+[.]", ".", basename(no_indiv))
      file.copy(no_indiv, file.path(out_, no_indiv_dest),
                copy.mode=FALSE)
    }
    if (length(indiv) > 0) {
      file.copy(indiv, file.path(out_, basename(indiv)),
                copy.mode=FALSE)
    }
    rm_dir(in_)
  }
}

rm_dir <- function(to_remove) {
  if (!is.null(to_remove) && dir.exists(to_remove)) {
    #system(sprintf("rm -r %s", to_remove))
    unlink(to_remove,recursive = TRUE, force = TRUE)
  }
}

#' Gets build-in report templates
#'
#' @return list of templates names
#' @export
#'
#' @examples
#' pmx_report_template()
pmx_report_template <- function() {
  system.file("rmarkdown", "templates", package = "ggPMX") %>%
    list.dirs(recursive = FALSE) %>%
    basename()
}

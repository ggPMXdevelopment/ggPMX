#' Generates ggpmX report from a pre-defined template
#'
#' @param ctr \code{pmxClass} controller
#' @param name \code{character} The report name
#' @param format \code{character} the result type, can be \cr
#' a standalone directory of plots or a report document as defined in the template \cr
#' (pdf, docx,..) ,or both
#' @param template \code{character} ggPMX predefined template or the 
#' path to a custom rmarkdwon template. \cr 
#' Use \code{\link{pmx_report_template}} to get the list 
#' of available templates

#' @param save_dir Output directory.
#' A directory to write the results files to
#' (defaults to the directory of the input file).
#' @param footnote \code{logical}  TRUE to add a footnote to the generated plots. The default footnote is to add \cr
#' the path where the plot is saved.
#' @param edit \code{logical}  TRUE to edit the template immediately
#' @param ... extra parameters depending in the template used
#' @export
#' @importFrom rmarkdown draft render
#' @importFrom knitr opts_chunk knit_hooks
#' @details
#' \code{pmx_report} uses pre-defined template .Rmd to generate the report.
#' The idea is to pass the controller as a report argument using knitr \code{params} artifact.
#' @example inst/examples/pmx_report.R

pmx_report <-
  function(ctr,
           name,
           save_dir=NULL,
           format=c("both","plots", "report"),
           template="standing",
           footnote=format == "both",
           edit=FALSE,
           ...) {
    
    assert_that(is_pmxclass(ctr))
    format <- match.arg(format)
    on.exit({
      remove_temp_files(ctr$save_dir)
      ctr$footnote <- FALSE
    })
    if (!is.null(save_dir)) {
      if (!dir.exists(save_dir)) {
        stop(sprintf("please provide a valid save directory : %s", save_dir))
      }
      save_dir <- path.expand(save_dir)
      ctr$save_dir <- tools::file_path_as_absolute(save_dir)
    }
    ctr$footnote <- footnote
    res <- pmx_draft(ctr, name, template, edit)
    standalone <- format %in% c("plots", "both")
    footnote <- format == "both" || footnote
    clean <- !standalone
    old_fig_process <- knitr::opts_chunk$get("fig.process")
    
    out_ <- file.path(ctr$save_dir, "ggpmx_GOF")
    rm_dir(out_)
    
    
    if (footnote || standalone) {
      dir.create(out_)
      
      pmx_fig_process_init(ctr)
      
      opts_chunk$set(
        fig.process = function(old_name) {
          pmx_fig_process(
            ctr = ctr,
            old_name = old_name,
            footnote = footnote,
            out_
          )
        }
      )
      
      suppressWarnings(render(
        res, "all", params = list(ctr = ctr, ...), envir = new.env(),
        output_dir = save_dir, clean = clean, quiet = TRUE
      ))
      
      knitr::opts_chunk$set(fig.process = old_fig_process)
      
      pmx_fig_process_wrapup(ctr)
      
      plot_dir <- sprintf("%s_files", name)
      in_ <- file.path(ctr$save_dir, plot_dir)
      rm_dir(in_)
      
      if (!clean) {
        ## create_ggpmx_gof(ctr$save_dir, name)
        remove_reports(format, ctr$save_dir)
      }
    }
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
    file.copy(old_name, new_name)
    return(new_name)
  }
  
  return(old_name)
}

pmx_draft <- function(ctr, name, template, edit) {
  template_file <- file.path(ctr$save_dir, sprintf("%s.Rmd", name))
  if (length(template_file) > 0 && file.exists(template_file)) {
    file.remove(template_file)
  }
  
  if (grepl(".Rmd", template) && !file.exists(template)) {
    stop(sprintf("!Template %s DO NOT EXIST", template))
  }
  if (file.exists(template)) {
    template_path <- system.file(
      "rmarkdown", "templates",
      "standing", package = "ggPMX"
    )
    temp_dir <- tempdir()
    invisible(file.copy(template_path, temp_dir, recursive = TRUE))
    dest_temp <- file.path(temp_dir, "standing", "skeleton", "skeleton.Rmd")
    invisible(file.copy(template, dest_temp, overwrite = TRUE))
    template_dir <- file.path(temp_dir, "standing")
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

remove_reports <- function(format, save_dir) {
  if (format == "plots") {
    invisible(file.remove(list.files(
      pattern = "(.pdf|.docx|Rmd)$", 
      path = save_dir, full.names = TRUE)))
  }
}

create_ggpmx_gof <- function(save_dir, name) {
  plot_dir <- sprintf("%s_files", name)
  if (dir.exists(file.path(save_dir, plot_dir))) {
    out_ <- file.path(save_dir, "ggpmx_GOF")
    rm_dir(out_)
    dir.create(out_)
    in_ <- file.path(save_dir, plot_dir)
    plots_ <- list.files(in_, recursive = TRUE, full.names = TRUE)
    
    idx <- grepl("^indiv", basename(plots_))
    indiv <- plots_[idx]
    no_indiv <- plots_[!idx]
    if (length(no_indiv) > 0) {
      no_indiv_dest <- gsub("[-]\\d+[.]", ".", basename(no_indiv))
      file.copy(no_indiv, file.path(out_, no_indiv_dest))
    }
    if (length(indiv) > 0) {
      file.copy(indiv, file.path(out_, basename(indiv)))
    }
    rm_dir(in_)
  }
}
rm_dir <- function(to_remove) {
  if (dir.exists(to_remove)) {
    system(sprintf("rm -r %s", to_remove))
  }
}

#' Gets build-in report templates
#'
#' @return list of templates names
#' @export
#'
#' @examples
#' pmx_report_template()
pmx_report_template <- function(){
  system.file("rmarkdown", "templates", package = "ggPMX") %>%
    list.dirs(recursive = FALSE) %>% 
    basename
}


to_ggplot2_names <- function(a) {
  z <- .globals$ggplot
  names(a)[names(a) %in% names(z)] <- z[names(z) %in% names(a)]
  a
}
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
#' Merge 2 lists
#'
#' left join , the first list is updated by the seond one
#' @param base_list  list to update
#' @param overlay_list list used to update the first list
#' @param recursive logical if TRUE do the merge in depth
#'
#' @return list
l_left_join <-
  function(base_list, overlay_list, recursive = TRUE) {
    if (length(base_list) == 0) {
      overlay_list
    } else if (length(overlay_list) == 0) {
      base_list
    } else {
      merged_list <- base_list
      for (name in names(overlay_list)) {
        base <- base_list[[name]]
        overlay <- overlay_list[[name]]

        if (is.list(base) && is.list(overlay) && recursive) {
          base <- to_ggplot2_names(base)
          overlay <- to_ggplot2_names(overlay)
          merged_list[[name]] <- l_left_join(base, overlay)
        } else {
          merged_list[[name]] <- NULL
          merged_list <-
            append(
              merged_list,
              overlay_list[which(names(overlay_list) %in%
                name)]
            )
        }
      }
      merged_list
    }
  }


## shiny:::anyUnnamed
anyUnnamed. <- function(x) {
  if (length(x) == 0) {
    return(FALSE)
  }
  nms <- names(x)
  if (is.null(nms)) {
    return(TRUE)
  }
  any(!nzchar(nms))
}

## shiny:::mergeVectors
mergeVectors. <- function(a, b) {
  if (anyUnnamed.(a) || anyUnnamed.(b)) {
    stop("Vectors must be either NULL or have names for all elements")
  }
  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}
## shiny:::dropNulls
dropNulls. <-
  function(x) {
    x[!vapply(x, is.null, FUN.VALUE = logical(1))]
  }


local_filter <-
  function(pmx_exp) {
    e <- if (is.character(pmx_exp)) {
      parse(text = pmx_exp)
    } else {
      as.expression(pmx_exp)
    }
    filter_ <- function(x) {
      r <- try(eval(parse(text = e), x), silent = TRUE)
      if (inherits(r, "try-error")) r <- TRUE
      x[r & !is.na(r)]
    }
  }


merge_defaults <-
  function(x, y) {
    c(x, y[setdiff(names(y), names(x))])
  }




#' Add draft layer annotation
#'
#' This function adds the word draft to certain graphics.
#' @param label draft layer default to DRAFT
#' @param size size of the annotation
#' @param colour color of the annotation default to grey50
#' @param x \code{numeric} x coordinate of the draft label
#' @param y \code{numeric} y coordinate of the draft label
#' @param ... extra parameters to geom text used to annotate the draft
#'
#' @return ggplot2 annotation

add_draft <- function(label = "DRAFT", size=10, colour="grey50", x = Inf, y = -Inf, ...) {
  do.call(annotate, list(
    geom = "text", label = label, size = size,
    colour = colour, family = "Courier",
    x = x, y = y,
    hjust = 1.2, vjust = -1.2, ...
  ))
}

#' Give the whole abbreviation definition
#'
#' @param param abbreviation term
#'
#' @return character abbreviation definition
#' @export
#' @examples
#' abbrev("VPC")
abbrev <- function(param) {
  keys_file <- file.path(
    system.file(package = "ggPMX"), "init",
    "abbrev.yaml"
  )
  keys <- yaml.load_file(keys_file)
  if (missing(param)) {
    keys
  } else {
    if (!is.null(keys[[param]])) keys[[param]] else param
  }
}


#' @import data.table
pmx_fread <- function(...) {
  fread(na.strings = c("NA", "."), ...)
}

is.formula <- function(x) {
  inherits(x, "formula")
}


#' Creates pmx controller using theophylline data
#' @param settings \code{pmxSettings} object
#' @param ...  other parameters of pmx_mlx like endpoint
#' @return pmx controller
#' @export
#'
#' @examples
#' \dontrun{
#' theophylline()
#' }
theophylline <- function(settings=NULL, ...) {
  theophylline <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "theophylline"
  )
  WORK_DIR <- file.path(theophylline, "Monolix")
  input_file <- file.path(theophylline, "data_pk.csv")
  vpc_file <- file.path(theophylline, "sim.csv")


  pmx_mlx(
    config = "standing",
    directory = WORK_DIR,
    input = input_file,
    dv = "Y",
    dvid = "dvid",
    cats = c("SEX"),
    conts = c("WT0", "AGE0"),
    strats = "STUD",
    settings = settings,
    sim = pmx_sim(
      file = vpc_file,
      irun = "rep",
      idv = "TIME"
    ),
    ...
  )
}

#' Creates pmx controller using monlix data having Occasional variable
#'
#' @return pmx controller
#' @export
#'
#' @examples
#' \dontrun{
#' pk_occ()
#' }
pk_occ <- function() {
  uc.name <- "1_popPK_model"
  data_file <- "PKdata_ggPMX.csv"

  uc.dir <- file.path(
    system.file(package = "ggPMX"), "testdata",
    uc.name
  )
  wd.mlx <- file.path(uc.dir, "RESULTS")
  input_file <- file.path(uc.dir, data_file)

  pmx_mlx(
    "standing",
    directory = wd.mlx,
    input = input_file,
    dv = "DV",
    dvid = "YTYPE",
    cats = c("SEX", "RACE", "DISE", "ILOW"),
    conts = c("AGE0", "WT0", "HT0", "TRT"),
    occ = "ISS"
  )
}


is_mlxtran <- function(file_name)
  identical(tools::file_ext(file_name), "mlxtran")

#' Parse MONOLIX mlxtran file
#'
#' @param file_name absolute path to mlxtran file
#'
#' @return \code{list} key/values to initialize ggPMX controller
#' @export
parse_mlxtran <- function(file_name) {
  on.exit(setwd(wd))

  wd <- getwd()
  section.name <- line <- section <- NULL
  sub_section <- sub_section.name <- NULL
  value <- NULL
  ## general file check
  if (!is_mlxtran(file_name)) {
    stop("this is not a mlxtran file")
  }
  if (!file.exists(file_name)) {
    stop("file do not exist")
  }
  lines <- readLines(file_name)

  ## empty lines
  firsts <- min(grep("<.*>", lines)) # first section
  lines <- lines[firsts:length(lines)]
  lines <- lines[lines != "" & !grepl(":$", lines)]
  ## extract sections
  sections <- cumsum(grepl("<.*>", lines))
  dat <- data.table(line = lines, section = sections)
  dat [, section.name := gsub("<|>", "", line[1]), section]
  dat <- dat[!grepl("<.*>", line)]
  ## extract sub sections
  dat[, sub_section := cumsum(grepl("\\[.*\\]", line)), section]
  dat[, sub_section := cumsum(grepl("\\[.*\\]", line)), section]
  dat[, sub_section.name := {
    if (sub_section > 0) {
      gsub("\\[|\\]", "", line[1])
    } else {
      "NO_SUB"
    }
  }, "section,sub_section"]
  dat <- dat[!grepl("\\[.*\\]", line)]
  dat[, c("section", "sub_section") := NULL]
  setnames(dat, c("sub_section.name", "section.name"), c("sub_section", "section"))
  dat <- dat[sub_section != "TASKS"]
  ## split lines

  dat[, c("key", "value") := tstrsplit(dat$line, " = ")]
  dat[, line := NULL]

  ## extract controller param
  ### directory
  export_path <- gsub("'", "", dat[key == "exportpath", value])
  directory <- file.path(dirname(file_name), export_path)
  if (!dir.exists(directory)) {
    directory <- file.path(dirname(file_name), "RESULTS")
  }
  if (!dir.exists(directory)) {
    stop("No results directory provided.")
  }
  ### input
  input <- gsub("'", "", dat[key == "file" & section == "DATAFILE", value])
  setwd(dirname(file_name))
  input <- normalizePath(input)
  ### dv
  dv <- dat[grepl("use=observation,", value), key]
  ### dvid
  dvid <- dat[grepl("use=observationtype", value, ignore.case = TRUE), key]

  ### cats
  cats <- dat[grepl("use=covariate, type=categorical", value), key]
  ### conts
  conts <- dat[grepl("use=covariate, type=continuous", value), key]
  ## occ
  occ <- dat[grepl("use=occasion", value), key]


  res <- list(
    directory = directory,
    input = input,
    dv = dv
  )
  if (length(cats) > 0) res$cats <- cats
  if (length(conts) > 0) res$conts <- conts
  if (length(occ) > 0) res$occ <- occ

  if (length(dvid) > 0) {
    res$dvid <- dvid
    aa <- dat[grepl("use=observation,", value), value]
    mlx16 <- grepl("ytype", aa)
    patt <- sprintf(".*name=(.*), %s=(.*), type.*", ifelse(mlx16, "ytype", "yname"))
    yname <- gsub(patt, "\\1;\\2", aa)
    yname <- unlist(strsplit(gsub("\\{|\\}|'| ", "", yname), ";"))
    ep <- if (length(yname) > 1) {
      if (grepl(",", yname[1])) {
        code <- strsplit(yname[2], ",")[[1]]
        files <- strsplit(yname[1], ",")[[1]]
        pmx_endpoint(code = code[1], file.code = ifelse(mlx16, code[1], files[1]))
      } else {
        pmx_endpoint(code = yname[2])
      }
    } else {
      pmx_endpoint(code = yname[1])
    }
    res$endpoint <- ep
  }
  res
}


#' Creates pkpd pmx controller using package internal data
#' @param code  can be 3 or 4
#' @export
pk_pd <- function(code = "3") {
  file.code <- switch(code,
    "3" = "1",
    "4" = "2"
  )



  pk_pd_path <- file.path(
    system.file(package = "ggPMX"), "testdata",
    "pk_pd"
  )
  WORK_DIR <- file.path(pk_pd_path, "RESULTS")
  ep <- pmx_endpoint(
    code,
    file.code = file.code
  )


  input_file <- file.path(pk_pd_path, "pk_pd.csv")

  ctr <- pmx_mlx(
    config = "standing",
    directory = WORK_DIR,
    input = input_file,
    dv = "dv",
    dvid = "dvid",
    cats = "sex",
    conts = "wt",
    endpoint = ep
  )
}


quantile <- function(...) do.call(stats::quantile, list(...))

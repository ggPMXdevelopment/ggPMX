# The ggPMX NONMEM reader (pmx_nm) is strongly based on NONMEM reading functions of the xpose package (v.0.4.11) (Thanks to Benjamin Guiastrennec)
#' NONMEM model file parser
#'
#' @description Parse NONMEM model files in R format
#'
#' @seealso \code{\link{pmx_read_nm_tables}}
#' @return A \code{\link[dplyr]{tibble}} of class \code{model} containing the following columns:
#' \itemize{
#'  \item \strong{problem}: a numeric identifier for the $PROBLEM associated with the code.
#'  \item \strong{level}: a unique numeric identifier to each subroutine block associated with the code.
#'  \item \strong{subroutine}: a character identifier named after the 3 first letters of the subroutine name e.g. '$THETA' and
#'  '$TABLE' will become 'the' and 'tab' respectively. In addition all output from the .lst is labeled 'lst', the general nonmem
#'  output e.g. NM-TRAN messages are labelled 'oth'. With priors thp, tpv, omp, opd, sip, spd abbreviations are given to the THETAP,
#'  THETAPV, OMEGAP, etc.
#'  \item \strong{code}: the code without comments or subroutine names e.g. '$THETA 0.5 ; TVCL' will return '0.5'.
#'  \item \strong{comment}: the last comment of a record e.g. '0.5 ; Clearance (L/h) ; TVCL' will return 'TVCL'.
#' }
#'
#' @param runno run number which is used for generating the model file name
#' @param prefix Prefix to be used to generate model file name. Used in combination with \code{runno} and \code{ext}.
#' @param ext Extension to be used to generate model file name. Should be one of'.lst' (default), '.out', '.res', '.mod' or '.ctl' for NONMEM.
#' @param file A character vector of path to the files or a \code{nm_table_list} object created with \code{list_nm_tables}.
#' @param dir directory of the model files.
#'
#' @examples
#' \dontrun{
#' # Using the `file` argument to import a model file:
#' nm_model <- pmx_read_nm_model(file = 'run001.lst', dir = 'models')
#'
#' # Using the `runno` argument to import a model file:
#' nm_model <- pmx_read_nm_model(runno = '001', ext = '.lst', dir = 'models')
#' }
#'

pmx_read_nm_model <- function(runno   = NULL,
                          prefix  = 'run',
                          ext     = '.lst',
                          file    = NULL,
                          dir     = NULL) {

  . <- NULL

  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }

  if (!is.null(runno)) {
    ext       <- pmx_make_extension(ext)
    full_path <- pmx_file_path(dir, stringr::str_c(prefix, runno, ext))
  } else {
    ext       <- pmx_get_extension(file)
    full_path <- pmx_file_path(dir, file)
  }

  if (!ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
    stop('NONMEM model file extension should be one lst, out, res, mod or ctl.', call. = FALSE)
  }

  if (!file.exists(full_path)) {
    stop('Model file ', basename(full_path), ' not found.', call. = FALSE)
  }

  model <- readr::read_lines(full_path)

  if (!any(stringr::str_detect(model, '^\\s*\\$PROB.+')) && ext %in% c('.lst', '.out', '.res')) {
    # Attempts to recover the model code from model file rather than in the nonmem output file
    full_path <- pmx_update_extension(full_path, c('.mod', '.ctl'))
    full_path <- full_path[file.exists(full_path)]

    if (any(file.exists(full_path))) {
      warning(c('No model code found in `', ext, '` NONMEM output file importing `',
                pmx_get_extension(full_path)[1], '` instead.'), call. = FALSE)
      model <- readr::read_lines(full_path[1])
    }
  }

  # Return error if input is bad
  if (!any(stringr::str_detect(model, '^\\s*\\$PROB.+'))) {
    stop(basename(full_path), ' is not a NONMEM model.', call. = FALSE)
  }

  model <- dplyr::tibble(code = model) %>%
    dplyr::filter(!stringr::str_detect(.$code, '^;[^;]*$|^$')) %>%
    dplyr::mutate(code = stringr::str_replace_all(.$code, '\\t+|\\s{2,}', ' ')) %>%
    dplyr::mutate(
      problem     = findInterval(seq_along(.$code), which(stringr::str_detect(.$code, '^\\s*\\$PROB.+'))),
      level       = findInterval(seq_along(.$code), which(stringr::str_detect(.$code, '^\\s*\\$.+'))),
      subroutine  = stringr::str_match(.$code, '^\\s*\\$(\\w+)')[, 2]) %>%
    tidyr::fill(dplyr::one_of('subroutine'))

  # Generate abbreviated subroutine names
  special <- c('THETAI', 'THETAR', 'THETAP', 'THETAPV', 'OMEGAP', 'OMEGAPD', 'SIGMAP', 'SIGMAPD')
  match_special <- match(model$subroutine[model$subroutine %in% special], special)
  model$subroutine[model$subroutine %in% special] <- c('thi', 'thr', 'thp', 'tpv',
                                                       'omp', 'opd', 'sip', 'spd')[match_special]
  model$subroutine <- stringr::str_extract(tolower(model$subroutine), '[a-z]{1,3}')

  # Format lst part
  if (any(stringr::str_detect(model$code, 'NM-TRAN MESSAGES'))) {
    lst_rows <- which(stringr::str_detect(model$code, 'NM-TRAN MESSAGES')):nrow(model)
    model[lst_rows,] <- model %>%
      dplyr::slice(lst_rows) %>%
      dplyr::mutate(problem = findInterval(seq_along(.$problem),
                                           which(stringr::str_detect(.$code, '^\\s*PROBLEM NO\\.:\\s*\\d+$')))) %>%
      dplyr::mutate(level = 1 + .$level[1] + .$problem,
                    subroutine = 'lst')
  }

  # Handle other special cases
  if (any(stringr::str_detect(model$code, '#CPUT'))) {
    cput_row <- which(stringr::str_detect(model$code, '#CPUT'))
    model[cput_row, 'problem'] <- 0
    model[cput_row:nrow(model), 'level'] <- model[cput_row:nrow(model), ]$level + 1
  }

  if (any(stringr::str_detect(model$code, 'Stop Time'))) {
    end_rows <- which(stringr::str_detect(model$code, 'Stop Time')):nrow(model)
    model[end_rows, 'problem'] <- 0
    model[end_rows, 'level'] <- model[end_rows[1], ]$level + 1
  }

  model[is.na(model$subroutine) | (model$problem == 0 & model$subroutine == 'lst'), 'subroutine'] <- 'oth'

  # Remove subroutine names from the code
  model$code <- stringr::str_replace(model$code, '^\\s*\\$\\w+\\s*', '')

  # Remove empty rows but $PROBLEM
  model <- model[!stringr::str_detect(model$code, '^(\\s|\\t)*$') | model$subroutine == 'pro', ]

  # Create comment column
  code_rows <- !model$subroutine %in% c('lst', 'oth') | model$level == 0
  model[code_rows, 'comment'] <- stringr::str_match(model[code_rows, ]$code, ';\\s*(.*)\\s*$')[, 2]
  model[code_rows, 'code'] <- stringr::str_replace(model[code_rows, ]$code, '\\s*;.*$', '')

  # Remove na values and output
  tidyr::replace_na(model, replace = list(code = '', comment = '')) %>%
    dplyr::select(dplyr::one_of(c('problem', 'level', 'subroutine', 'code', 'comment'))) %>%
    dplyr::mutate(problem = as.integer(.$problem),
                  level   = as.integer(.$level)) %>%
    structure(file     = basename(full_path),
              dir      = dirname(full_path),
              software = 'nonmem',
              class    = c('nm_model', class(.)))
}

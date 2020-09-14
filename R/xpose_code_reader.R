#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R. This function automatically 
#' detects the optimal settings to import the tables from nonmem.
#'
#' @param file A character vector of path to the files or a \code{nm_table_list} object created with \code{list_nm_tables}.
#' @param dir Location of the model files.
#' @param combined Logical value indicating whether multiple tables should be combined into a single one. If the number of rows 
#' does not match an error will be returned.
#' @param rm_duplicates Logical value indicating whether duplicated columns should be removed.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param simtab If \code{TRUE} only reads in simulation tables, if \code{FALSE} only reads estimation tables. 
#' Default \code{NULL} reads all tables.
#' @param ziptab If \code{TRUE} search for the tables that have been compressed and renamed ??<file>.zip'.
#' @param ... Additional arguments to be passed to the \code{\link[readr]{read_table2}} or \code{\link[readr]{read_csv}} functions.
#' 
#' @section Table format requirement:
#' When using \code{read_nm_tables} with the \code{combined} argument set to \code{FALSE} an \code{ID} column 
#' must be present in all data tables. When \code{combined} is set to \code{TRUE} instead an \code{ID} column must be 
#' present in at least one table for each problem and for each `firstonly` category. \code{ID} columns are required 
#' to properly combine/merge tables and removing \code{NA} records. If the \code{ID} column is missing from a table and 
#' \code{combined = FALSE} \code{read_nm_tables} will return the following warning: \code{Unknown variables: `ID`}. While
#' the data is returned beware that \code{NA} records might be left in the data and the output should be checked carefully.
#' If \code{combined = TRUE} \code{read_nm_tables} xpose is more strict and will return the following warning instead: 
#' \code{Dropped `<tablenames>` due to missing required `ID` column.}.
#' 
#' @examples
#' \dontrun{
#' # Import tables manually and return them as a list of individual tables
#' nm_tables <- read_nm_tables(file = c('sdtab001', 'patab001'), 
#'                             dir = 'models', combined = FALSE)
#' 
#' # Import tables manually and return them as a single merged table
#' nm_tables <- read_nm_tables(file = c('sdtab001', 'patab001'), 
#'                             dir = 'models', combined = TRUE)
#' 
#' # Import tables automatically (used internally by xpose_data())
#' nm_tables <- read_nm_model(file = 'run001.lst', dir = 'models') %>% 
#'               list_nm_tables() %>% 
#'               read_nm_tables()
#' 
#' # Passing arguments to readr via `...` 
#' # (e.g. import columns as character and only first 10 rows)
#' nm_tables <- read_nm_tables(file = 'sdtab001', dir = 'models', 
#'                             col_type = readr::cols(.default = 'c'), 
#'                             n_max = 10)
#' 
#' }
#' @export
read_nm_tables <- function(file          = NULL,
                           dir           = NULL,
                           combined      = TRUE,
                           rm_duplicates = TRUE,
                           quiet         = FALSE,
                           simtab        = NULL,
                           ziptab        = TRUE,
                           user_mode     = TRUE,
                           ...) {
  # Check inputs
  if (is.null(file)) stop('Argument `file` required.', call. = FALSE)
  
  if (!is.null(file) && !is.nm.table.list(file)) {
    file <- dplyr::tibble(problem   = 1, 
                          file      = file_path(dir, file),
                          firstonly = FALSE,
                          simtab    = FALSE)
  }
  
  if(user_mode){
    user_mode <- !is.nm.table.list(file)
  }

  
  # Filter tables if needed
  if (!is.null(simtab)) file <- file[file$simtab == simtab, ]
  msg('\nLooking for nonmem output tables.', quiet)
  
  # Check that file exists
  if (is.null(file) || !any(file.exists(file$file))) {
    stop('No table files could be found.', call. = FALSE)
  }
  
  if (any(duplicated(file$file))) {
    stop('No table imported due to duplicated names.', call. = FALSE)
  }
  
  tables <- file[file.exists(file$file), ]
  
  # Search for compressed tables
  if (ziptab) { 
    tables_zip <- file[!file.exists(file$file), ]
    if (nrow(tables_zip) > 0) {
      tables_zip$file <- stringr::str_c(tables_zip$file, '.zip')
      tables_zip <- tables_zip[file.exists(tables_zip$file), ]
      if (nrow(tables_zip) > 0) {
        tables <- tables %>% 
          dplyr::bind_rows(tables_zip) %>% 
          dplyr::arrange_at(.vars = c('problem', 'file'))
      }
    }
  }
  
  # Print reading messages
  tables %>% 
    dplyr::mutate(grouping = 1:dplyr::n(),
                  name = stringr::str_c(basename(.$file), dplyr::if_else(.$firstonly, ' (firstonly)', ''))) %>% 
    dplyr::group_by_at(.vars = c('problem', 'simtab')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(string = purrr::map_chr(.$data, ~stringr::str_c(.$name, collapse = ', '))) %>% 
    {stringr::str_c(.$string, ' [$prob no.', .$problem, dplyr::if_else(.$simtab, ', simulation', ''), 
                    ']', collapse = '\n         ')} %>% 
    {msg(c('Reading: ', .), quiet)}
  
  # Collect options for table import
  tables <- tables %>% 
    dplyr::mutate(top = purrr::map(.$file, ~readr::read_lines(file = ., n_max = 3)),
                  grouping = 1:dplyr::n()) %>% 
    dplyr::group_by_at(.vars = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(args = purrr::map(.x = .$data, .f = read_args, quiet, ...)) %>% 
    tidyr::unnest(dplyr::one_of('data')) %>% 
    tidyr::unnest(dplyr::one_of('args')) %>% 
    dplyr::mutate(name = basename(.$file)) %>% 
    dplyr::select(dplyr::one_of('problem', 'name', 'simtab', 'firstonly', 'fun', 'params'))
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Read in data
  tables <- tables %>% 
    dplyr::bind_cols(tables %>% 
                       dplyr::select(dplyr::one_of(c('fun', 'params'))) %>% 
                       {purrr::invoke_map(.f = .$fun, .x = .$params)} %>%
                       dplyr::tibble(data = .))
  
  if (!combined) {
    return(purrr::set_names(x = purrr::map(tables$data, ~tidyr::drop_na(., dplyr::one_of('ID'))),
                            nm = tables$name))
  }
  
  # Index datasets
  tables <- tables %>% 
    dplyr::mutate(grouping = 1:dplyr::n()) %>% 
    dplyr::group_by_at(.vars = 'grouping')
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
  } else {
    tables <- tables %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  tables <- tables %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(index = purrr::map(.$tmp, index_table),
                  nrow =  purrr::map_dbl(.$tmp, ~nrow(.$data[[1]]))) %>% 
    tidyr::unnest(dplyr::one_of('tmp')) %>% 
    dplyr::ungroup()
  
  
  # Combine tables with same number of rows
  tables <- tables %>% 
    dplyr::group_by_at(.vars = c('problem', 'simtab', 'firstonly'))
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('problem', 'simtab', 'firstonly'))
  } else {
    tables <- tables %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  tables <- tables %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(out = purrr::map(.$tmp, combine_tables)) %>% 
    tidyr::unnest(dplyr::one_of('out')) %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'firstonly', 'data', 'index'))
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Remove duplicated columns to decrease xpdb size
  if (rm_duplicates) {
    tables <- tables %>% 
      dplyr::mutate(grouping = 1:dplyr::n()) %>% 
      dplyr::group_by_at(.vars = 'grouping')
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
    } else {
      tables <- tables %>% tidyr::nest(.key = 'tmp')
    }
    ## END TEMP
    
    tables <- tables %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(out = purrr::map(.$tmp, ~dplyr::select(.$data[[1]], 
                                                           dplyr::one_of(unique(unlist(.$index[[1]]$col)))))) %>% 
      tidyr::unnest(dplyr::one_of('tmp')) %>% 
      dplyr::select(dplyr::one_of('problem', 'simtab', 'firstonly', 'index', 'out')) %>% 
      dplyr::rename(!!rlang::sym('data') := dplyr::one_of('out'))
  }
  
  # Merge firsonly tables with main tables
  if (any(tables$firstonly)) {
    msg('Consolidating tables with `firstonly`', quiet)
    tables <- tables %>%
      dplyr::group_by_at(.vars = c('problem', 'simtab'))
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('problem', 'simtab'))
    } else {
      tables <- tables %>% tidyr::nest(.key = 'tmp')
    }
    ## END TEMP
    
    tables <- tables %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(out = purrr::map(.$tmp, merge_firstonly, quiet)) %>% 
      tidyr::unnest(dplyr::one_of('out')) %>% 
      dplyr::select(dplyr::one_of('problem', 'simtab', 'data', 'index'))
  }
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Convert catcov, id, occ, dvid to factor
  tables <- tables %>% 
    dplyr::mutate(grouping = .$problem) %>% 
    dplyr::group_by_at(.vars = 'grouping')
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
  } else {
    tables <- tables %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  tables <- tables %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(tmp = purrr::map(.$tmp, function(x) {
      col_to_factor <- colnames(x$data[[1]]) %in% 
        x$index[[1]]$col[x$index[[1]]$type %in% c('catcov', 'id', 'occ', 'dvid')]
      x$data[[1]] <- dplyr::mutate_if(x$data[[1]], col_to_factor, as.factor)
      x
    })) %>% 
    tidyr::unnest(dplyr::one_of('tmp')) %>% 
    dplyr::mutate(modified = FALSE) %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'data', 'modified'))
  
  # If user mode return simple tibble as only 1 problem should be used
  if (user_mode) return(tables$data[[1]])
  tables
}


#' Define data import functions
#' 
#' @param fun Abbreviated `readr` data import function. 
#' Can be `csv`, `csv2` or `table`.
#' 
#' @return A data import function.
#' 
#' @keywords internal
#' @export
read_funs <- function(fun) {
  c(csv   = readr::read_csv,
    csv2  = readr::read_csv2,
    table = readr::read_table2)[fun]
}


#' Define data import arguments
#' 
#' @param x A list containing a the 3 first records of a 
#' dataset under `[[1]]`.
#' @param quiet Should messages be displayed to the console.
#' @param col_types Defines the type of each column to be passed to 
#' the `readr` import function.
#' @param na Character string defining the values to be treated as `NA`.
#' @param comment Character string defining the value to mark comments.
#' @param skip Number of rows to be skipped before reading the data.
#' @param ... Additional arguments to be passed to the `readr` function
#' 
#' @return A list of 2 levels fun (the import function) and params (a list 
#' of arguments to be used when calling fun).
#' 
#' @keywords internal
#' @export
read_args <- function(x, quiet, col_types = readr::cols(.default = 'd'), 
                      na = 'NA', comment = 'TABLE', skip = 1, ...) {
  
  top <- x$top[[1]]
  
  if (is.na(top[3]) || !stringr::str_detect(top[3], '\\d+E[+-]\\d+\\s*')) {
    warning(c('Dropped: ', basename(x$file), ' due to unexpected data format'), call. = FALSE)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  fun <- dplyr::case_when(stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s*;') ~ 'csv2',
                          stringr::str_detect(top[3], '\\d.\\d+E[+-]\\d+\\s*,') ~ 'csv', 
                          TRUE ~ 'table')
  
  skip_h <- dplyr::if_else(stringr::str_detect(top[1], 'TABLE NO\\.\\s+\\d'), 1, 0)
  
  if (!stringr::str_detect(top[1 + skip_h], '[A-z]{2,}+')) {
    warning(c('Dropped: ', basename(x$file), ' due to missing headers.'), call. = FALSE)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  col_names <- top[1 + skip_h] %>% 
    stringr::str_trim(side = 'both') %>% 
    stringr::str_split(pattern = dplyr::case_when(fun == 'csv' ~ ',', 
                                                  fun == 'csv2' ~ ';',
                                                  fun == 'table' ~ '\\s+')) %>% 
    purrr::flatten_chr() %>% 
    stringr::str_trim()
  
  dplyr::tibble(fun = read_funs(fun),
                params = list(list(file = x$file, skip = skip, comment = comment, 
                                   na = c(na, col_names), col_names = col_names, 
                                   col_types = col_types, ...)))
}


#' Combine tables
#' 
#' @param x A list containing the tables (`x$data`) to be 
#' combined, their names (`x$name`), their number of rows (`x$nrow`) 
#' and their respective index (`x$index`).
#' 
#' @return A list containing `data` and `index` of the combined table.
#' 
#' @keywords internal
#' @export
combine_tables <- function(x) {
  # Check for matching length
  if (length(unique(x$nrow)) > 1) {
    warning(c('Dropped ', stringr::str_c('`', x$name, '`', collapse = ', '), 
              ' due to missmatch in row number.'), call. = FALSE)
    return(dplyr::tibble(data = list(), index = list()))
    
  }
  
  # Check for ID column
  if (!any(purrr::map_lgl(x$index, ~any(.$type == 'id')))) {
    warning(c('Dropped ', stringr::str_c('`', x$name, '`', collapse = ', '), 
              ' due to missing required `ID` column.'), call. = FALSE)
    return(dplyr::tibble(data = list(), index = list()))
  }
  
  # Prepare the combined data
  # Note: here the data may contain duplicated columns so 
  #       we de-duplicate them in base R to avoid errors 
  #       with the tidyverse.
  tmp_df <- do.call("cbind", unname(x$data))
  tmp_df <- tmp_df[, which(!duplicated(names(tmp_df))) ] %>% 
    tibble::as_tibble() %>% 
    tidyr::drop_na(dplyr::one_of('ID')) %>%
    list()
  
  # Combine tables
  dplyr::tibble(data  = tmp_df,
                index = list(dplyr::bind_rows(x$index)))
}


#' Merge firstonly table with full length tables
#' 
#' @param x A list containing the tables (`x$data`) to be 
#' merged, the firstonly flag (`x$firstonly`) and the 
#' indexes (`x$index`).
#' @param quiet Should messages be displayed to the console.
#' 
#' @return A list containing `data` and `index` of the merged table.
#' 
#' @keywords internal
#' @export
merge_firstonly <- function(x, quiet) {
  if (nrow(x) == 1) {
    # No merge needed
    return(dplyr::tibble(data = x$data, index = x$index))
  } else if (nrow(x) != 2) {
    warning(c(' * Something went wrong while consolidating: ', 
              stringr::str_c(x[x$firstonly == TRUE, ]$index[[1]]$tables, 
                             collapse = ', ')), call. = FALSE) 
    return(dplyr::tibble(data = list(), index = list()))
  }
  xdata   <- x$data[x$firstonly == FALSE][[1]]
  ydata   <- x$data[x$firstonly == TRUE][[1]]
  by_vars <- intersect(colnames(xdata), colnames(ydata))
  msg(c(' * Joining by: ', stringr::str_c(by_vars, collapse = ', ')), quiet)
  dplyr::tibble(data = list(dplyr::left_join(x  = xdata, 
                                             y  = ydata,
                                             by = by_vars)),
                index = x$index %>% 
                  dplyr::bind_rows() %>% 
                  list())
}


#' Index table columns
#' 
#' @param x A list containing the tables (`x$data`) to be 
#' combined along with their respective names (`x$name`).
#' 
#' @return A tibble of the index.
#' 
#' @keywords internal
#' @export
index_table <- function(x) {
  tab_type <- dplyr::case_when(
    stringr::str_detect(x$name, 'patab') ~ 'param',   # model parameters
    stringr::str_detect(x$name, 'catab') ~ 'catcov',  # categorical covariate
    stringr::str_detect(x$name, 'cotab') ~ 'contcov', # continuous covariate
    TRUE ~ 'na')
  
  x$data[[1]] %>% 
    colnames() %>% 
    dplyr::tibble(table = x$name,
                  col   = ., 
                  type  = NA_character_, 
                  label = NA_character_,     # Feature to be added in future releases
                  units = NA_character_) %>% # Feature to be added in future releases
    dplyr::mutate(type = dplyr::case_when(
      .$col == 'ID' ~ 'id',
      .$col == 'DV' ~ 'dv',
      .$col == 'TIME' ~ 'idv',
      .$col == 'OCC' ~ 'occ',
      .$col == 'DVID' ~ 'dvid',
      .$col == 'AMT' ~ 'amt',
      .$col == 'MDV' ~ 'mdv',
      .$col == 'EVID' ~ 'evid',
      .$col == 'IPRED' ~ 'ipred',
      .$col == 'PRED' ~ 'pred',
      .$col %in% c('RES', 'WRES', 'CWRES', 'IWRES', 'EWRES', 'NPDE') ~ 'res',
      stringr::str_detect(.$col, 'ETA\\d+|ET\\d+') ~ 'eta',
      stringr::str_detect(.$col, '^A\\d+$') ~ 'a',
      TRUE ~ tab_type))
}


is.nm.table.list <- function(x) {
  inherits(x, 'nm_table_list')
}

file_path <- function(dir, file) {
  if (is.null(dir)) return(file) 
  
  # Remove trailing forward slash
  dir <- stringr::str_replace(dir, '\\/+$', '')
  file.path(dir, file)
}



tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}

read_nm_model <- function(runno   = NULL,
                          prefix  = 'run',
                          ext     = '.lst',
                          file    = NULL,
                          dir     = NULL) {
  
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (!is.null(runno)) {
    ext       <- make_extension(ext)
    full_path <- file_path(dir, stringr::str_c(prefix, runno, ext))
  } else {
    ext       <- get_extension(file)
    full_path <- file_path(dir, file)
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
    full_path <- update_extension(full_path, c('.mod', '.ctl'))
    full_path <- full_path[file.exists(full_path)]
    
    if (any(file.exists(full_path))) {
      warning(c('No model code found in `', ext, '` NONMEM output file importing `', 
                get_extension(full_path)[1], '` instead.'), call. = FALSE)
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
    dplyr::select(dplyr::one_of('problem', 'level', 'subroutine', 'code', 'comment')) %>% 
    dplyr::mutate(problem = as.integer(.$problem),
                  level   = as.integer(.$level)) %>% 
    structure(file     = basename(full_path),
              dir      = dirname(full_path),
              software = 'nonmem',
              class    = c('nm_model', class(.)))
}


get_extension <- function(x, dot = TRUE) {
  x <- stringr::str_extract(x, '\\.[[:alnum:]]+$')
  x[is.na(x)] <- ''
  if (!dot) x <- stringr::str_replace_all(x, '\\.', '')
  x
}



#' List NONMEM output tables
#'
#' @description List NONMEM output tables file names from a \code{nm_model} object.
#'
#' @param nm_model An xpose nm_model object generated with \code{\link{read_nm_model}}.
#'
#' @seealso \code{\link{read_nm_model}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' read_nm_model(file = 'run001.lst') %>% 
#'   list_nm_tables()
#' }
#' 
#' @export
list_nm_tables <- function(nm_model = NULL) {
  
  if (is.null(nm_model) || !is.nm.model(nm_model)) {
    stop('Object of class `nm_model` required.', call. = FALSE)
  }
  
  # Prepare null object to be returned if no $table is found
  null_object <- as.nm.table.list(dplyr::tibble(problem = -1, file = '', 
                                                firstonly = NA, simtab = NA))
  
  # Get NM code associated with the tables
  table_list <- nm_model %>% 
    dplyr::filter(.$problem > 0, .$subroutine == 'tab') 
  
  if (nrow(table_list) == 0) return(null_object)
  
  table_list <- table_list %>% 
    dplyr::group_by_at(.vars = c('problem', 'level')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(string = purrr::map_chr(.$data, ~stringr::str_c(.$code, collapse = ' '))) %>% 
    dplyr::mutate(file = stringr::str_match(.$string, '\\s+FILE\\s*=\\s*([^\\s]+)')[, 2]) %>% 
    dplyr::filter(!is.na(.$file))
  
  if (nrow(table_list) == 0) return(null_object)
  
  # Find table names and firstonly option
  table_list <- table_list %>% 
    dplyr::mutate(file = file_path(attr(nm_model, 'dir'), .$file),
                  firstonly = stringr::str_detect(.$string, 'FIRSTONLY')) %>% 
    dplyr::select(dplyr::one_of('problem', 'file', 'firstonly'))
  
  # Prep simtab flag
  sim_flag <- nm_model %>% 
    dplyr::filter(.$problem > 0) %>% 
    dplyr::group_by_at(.vars = 'problem') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(simtab = purrr::map_lgl(.$data, ~!any(stringr::str_detect(.$subroutine, 'est')))) %>% 
    dplyr::select(dplyr::one_of(c('problem', 'simtab')))
  
  # Merge and output
  table_list %>% 
    dplyr::left_join(sim_flag, by = 'problem') %>% 
    as.nm.table.list()
}

is.nm.model <- function(x) {
  inherits(x, 'nm_model')
}

as.nm.table.list <- function(x) {
  if (!is.nm.table.list(x)) {
    structure(x, class = c('nm_table_list', class(x)))
  } else {
    x
  }
}

#' Manually define nonmem tables to be imported
#' 
#' @description Manually provide names of the table files to be imported by \code{xpose_data}.
#'
#' @param tab_names Provide the name of the tables to import e.g. 'sdtab', 'patab', 'cotab', 
#' 'catab' for NONMEM.
#' @param tab_suffix Default is '', but can be changed to any character string to be used as 
#' suffix in the table names.
#' @param sim_suffix Default is 'sim', but can be changed to any character string to be used as 
#' suffix in the simulation table names e.g. sdtab001sim.
#'
#' @details 
#' In order to be imported manually, table names must follow the following convention: 
#' \code{<tab_names><runno><tab/sim_suffix>} e.g. sdtab001sim. When the argument `file` is used in 
#' \code{xpose_data}, the \code{<runno>} part is guessed by taking the portion of the string starting 
#' by any digit and ending at the file extension e.g. \code{file = run001a.mod} will guess <runno> as
#' `001a`. If no valid <runno> can be guessed, xpose will return an error. In this case it is advised 
#' to use the \code{xpose_data} argument `runno` directly rather than `file` hence preventing xpose 
#' from having to guess <runno>.
#' 
#' Note that with manual table import xpose still reads in the NONMEM model file in order to generate
#' the run summary.
#' 
#' @seealso \code{\link{xpose_data}}
#' @examples 
#' \dontrun{
#' # Import all names specified by default as in xpose4
#' xpose_data(runno = '001', manual_import = manual_nm_import())
#' 
#' # Import a specific table name
#' xpose_data(runno = '001', manual_import = manual_nm_import(tab_names = 'mytab'))
#' }
#' @export
manual_nm_import <- function(tab_names = c('sdtab', 'mutab', 'patab', 'catab', 'cotab', 
                                           'mytab', 'extra', 'xptab', 'cwtab'),
                             tab_suffix = '', sim_suffix = 'sim') {
  
  list(tab_suffix = tab_suffix, sim_suffix = sim_suffix, tab_names = tab_names)
}


#' Creates an nm_table_list from manually defined table name patterns
#' 
#' @param runno Run number to be used to generate model file name.
#' @param file Model file name containing the file extension.
#' @param dir Location of the model files.
#' @param tab_list A list of table definition generated by `manual_nm_import`.
#' 
#' @return A `nm_table_list`
#' 
#' @keywords internal
#' @export
list_nm_tables_manual <- function(runno = NULL, file = NULL, dir = NULL, tab_list) {
  if (is.null(runno)) {
    # Attempt to guess runno if file has been used
    runno <- stringr::str_match(string = update_extension(file, ''), 
                                pattern = '\\d.+$')[1,]
    if (is.na(runno)) {
      stop('Failed to guess `runno` from `file` argument. Check ?manual_nm_import for help.',
           call. = FALSE)
    }
  }
  file_path(dir, stringr::str_c(tab_list$tab_names, runno)) %>% 
    dplyr::tibble(problem = 1, file = ., firstonly = FALSE, simtab = NA) %>% 
    tidyr::expand(problem = .$problem, file = .$file, firstonly = .$firstonly, simtab = c(FALSE, TRUE)) %>% 
    dplyr::mutate(file = dplyr::if_else(.$simtab, stringr::str_c(.$file, tab_list$sim_suffix),
                                        stringr::str_c(.$file, tab_list$tab_suffix))) %>% 
    dplyr::filter(file.exists(.$file)) %>% 
    as.nm.table.list()
}

make_extension <- function(x) {
  dplyr::if_else(!stringr::str_detect(x, '^\\..+'), stringr::str_c('.', x), x)
}



update_extension <- function(x, ext) {
  stringr::str_replace(x, '\\.[[:alnum:]]+$', ext)
}



#' NONMEM output file import function
#'
#' @description Quickly import NONMEM output files into R.
#'
#' @param runno Run number to be evaluated.
#' @param prefix Prefix of the model file names.
#' @param ext A vector of the file extension to import. By default '.ext', '.cor', '.cov', '.phi', '.grd', '.shk'
#' files are listed.
#' @param file Names of the model output file to be imported. Alternative argument to \code{prefix},
#' \code{runno} and \code{ext}.
#' @param dir Location of the model files.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @inheritSection xpose_data File path generation
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' # Using the `file` argument to import a model file:
#' ext_file <- read_nm_files(file = 'run001.ext', dir = 'models')
#' 
#' # Using the `runno` argument to import a model file:
#' ext_file <- read_nm_files(runno = '001', ext = '.ext', dir = 'models')
#' }
#' @export
read_nm_files <- function(runno  = NULL,
                          prefix = 'run',
                          ext    = c('.ext', '.cor', '.cov', '.phi', '.grd', '.shk'),
                          file   = NULL,
                          dir    = NULL,
                          quiet  = FALSE) {
  
  # Check inputs
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (missing(quiet)) quiet <- !interactive()
  
  # Generate full paths
  if (!is.null(runno)) {
    full_path <- file_path(dir, stringr::str_c(prefix, runno, make_extension(ext)))
  } else {
    full_path <- file_path(dir, file)
  }
  
  full_path <- sort(unique(full_path))
  bases     <- basename(full_path)
  
  msg('\nLooking for nonmem output files', quiet)
  
  if (!any(file.exists(full_path))) {
    stop('No output files could be found.', call. = FALSE)
  }
  
  msg(c('Reading: ', stringr::str_c(bases[file.exists(full_path)], collapse = ', ')), quiet)
  
  out <- full_path %>% 
    dplyr::tibble(path = ., name = basename(.)) %>% 
    dplyr::filter(file.exists(.$path)) %>% 
    dplyr::mutate(grouping = 1:dplyr::n(),
                  raw = purrr::map(.$path, .f = readr::read_lines)) %>% 
    dplyr::group_by_at(.vars = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(tmp = purrr::map(.$data, .f = parse_nm_files, quiet)) %>% 
    dplyr::mutate(drop = purrr::map_lgl(.$tmp, is.null)) 
  
  if (all(out$drop)) stop('No output file imported.', call. = FALSE)
  
  out %>% 
    dplyr::filter(!.$drop) %>% 
    tidyr::unnest(dplyr::one_of('data')) %>% 
    tidyr::unnest(dplyr::one_of('tmp')) %>% 
    dplyr::mutate(extension = get_extension(.$name, dot = FALSE),
                  modified = FALSE) %>% 
    dplyr::select(dplyr::one_of('name', 'extension', 'problem', 'subprob', 
                                'method', 'data', 'modified'))
}


#' Parse NONMEM output files
#' 
#' @description Function parsing NONMEM output files from their 
#' raw input.
#' 
#' @param dat A list containing the raw data as vector of strings (`dat$raw`)
#' and their respective file names (`dat$name`).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @return A tibble containing the parsed `data` along with `name`, `problem`, 
#' `subprob`, and `method`.
#' 
#' @keywords internal
#' @export
parse_nm_files <- function(dat, quiet) {
  if (length(unlist(dat$raw)) == 0) {
    tab_rows <- NULL 
  } else {
    x <- dplyr::tibble(
      raw = unlist(dat$raw), 
      problem = NA_character_, 
      subprob = NA_character_, 
      method  = NA_character_, 
      header  = FALSE
    )
    tab_rows <- which(stringr::str_detect(x$raw, '^\\s*TABLE NO'))
  }
  
  if (length(tab_rows) == 0) {
    warning(c('Dropped `', dat$name, '` due to inappropriate format.'), call. = FALSE)
    return()
  }
  
  x[tab_rows, ]$problem <- stringr::str_match(x[tab_rows, ]$raw, '\\s+Problem=(\\d+)')[,2]
  x[tab_rows, ]$subprob <- stringr::str_match(x[tab_rows, ]$raw, '\\s+Subproblem=(\\d+)')[,2]
  x[tab_rows, ]$method  <- dplyr::case_when(stringr::str_detect(x[tab_rows, ]$raw, 'First Order Conditional') ~ 'foce',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'First Order') ~ 'fo',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Laplacian Conditional') ~ 'lce', 
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Iterative Two Stage') ~ 'its',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Importance Sampling') ~ 'imp',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Stochastic Approximation') ~ 'saem',
                                            stringr::str_detect(x[tab_rows, ]$raw, 'Markov-Chain') ~ 'bayes',
                                            TRUE ~ 'na')
  
  # Assumes that header are always present
  x[tab_rows + 1, ]$header <- TRUE
  
  # Guess column separator from the first problem only
  sep <- dplyr::case_when(
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ';[A-z]+') ~ ';[A-z]|[A-z];',
    stringr::str_detect(x[tab_rows[1] + 1, ]$raw, ',[A-z]+') ~ ',[A-z]|[A-z],',
    TRUE ~ '\\s+')
  
  x %>% 
    tidyr::fill(dplyr::one_of('problem', 'subprob', 'method')) %>% 
    dplyr::slice(-tab_rows) %>%
    dplyr::mutate(problem = as.numeric(.$problem),
                  subprob = as.numeric(.$subprob),
                  raw = stringr::str_trim(.$raw, side = 'both')) %>% 
    dplyr::group_by_at(.vars = c('problem', 'subprob', 'method')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(data = purrr::map(.$data, 
                                    .f   = raw_to_tibble, 
                                    sep  = sep, 
                                    file = dat$name))
}  


#' Convert raw strings to tibble
#' 
#' @description Convert raw data strings to a tibble format.
#' 
#' @param x A list containing the raw data as vector of strings (`x$raw`).
#' @param sep A separator which will be used to create columns.
#' @param file The name of the file to be parsed.
#' 
#' @return A tibble.
#' 
#' @keywords internal
#' @export
raw_to_tibble <- function(x, sep, file) {
  header <- x$raw[x$header] %>% 
    stringr::str_split(pattern = sep) %>% 
    purrr::flatten_chr()
  
  if (any(is.na(header))) {
    warning(c('Issue encountered while parsing ', file, '.'), call. = FALSE)
    return()
  }
  
  x[!x$header, ] %>%   
    tidyr::separate(col = 'raw', sep = sep, into = header) %>% 
    dplyr::select(-ncol(.)) %>% 
    dplyr::mutate_if(colnames(.) != 'NAME', as.numeric)
}

msg <- function(txt, quiet = TRUE) {
  if (!quiet) message(txt)
}


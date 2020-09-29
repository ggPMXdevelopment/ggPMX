# The ggPMX NONMEM reader (pmx_nm) is strongly based on NONMEM reading functions of the xpose package (v.0.4.11) (Thanks to Benjamin Guiastrennec)
#
#' List NONMEM output tables
#'
#' @description List NONMEM output tables file names from a \code{nm_model} object.
#'
#' @param nm_model An nm_model object generated with \code{\link{read_nm_model}}.
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

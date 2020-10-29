# The ggPMX NONMEM reader (pmx_nm) is strongly based on NONMEM reading functions of the xpose package (v.0.4.11) (Thanks to Benjamin Guiastrennec)
# Utils needed for the pmx_nm()

#' Test for nm_table_list class
#' 
#' @description Reports whether x is a `nm_table_list` object
#' 
#' @param x An object to be tested.
#' 
#' @return Logical value, `TRUE` for `nm_table_list` class 
#' and `FALSE` otherwise.
#' 
#' @keywords internal
pmx_is.nm.table.list <- function(x) {
  inherits(x, 'nm_table_list')
}


#' Generate clean file paths
#' 
#' @description Wrapper around `file.path` that cleans trailing forward 
#' slash and missing `dir`.
#' 
#' @param dir A string or vector of strings containing the directory path.
#' @param file A file name or vector containing the file names.
#' 
#' @return A string or vector of string of the full file path.
#' 
#' @keywords internal
pmx_file_path <- function(dir, file) {
  if (is.null(dir)) return(file) 
  
  # Remove trailing forward slash
  dir <- stringr::str_replace(dir, '\\/+$', '')
  file.path(dir, file)
}


#' Check tidyr version
#' 
#' @description Check the version of tidyr to handle the gap between v0.8.3 and v1.0.0
#' 
#' @return the package version of tidyr
#' 
#' @keywords internal
pmx_tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}

#' Get file extension
#' 
#' @description Extract file extension from the filename string.
#' 
#' @param x A string or vector of strings containing the filenames with the extension.
#' @param dot Logical, if `TRUE` the returned value will contain the 
#' dot (e.g `.mod`) else only the extension itself will be returned (e.g. `mod`).
#' 
#' @return A string or vector of string of the file(s) extension.
#' 
#' @keywords internal
pmx_get_extension <- function(x, dot = TRUE) {
  x <- stringr::str_extract(x, '\\.[[:alnum:]]+$')
  x[is.na(x)] <- ''
  if (!dot) x <- stringr::str_replace_all(x, '\\.', '')
  x
}

#' Generate extension string
#' 
#' @description Generate consistent extension strings by adding dot 
#' prefix whenever necessary.
#' 
#' @param x A string or vector of strings containing the extension to be standardized.
#' 
#' @return A string or vector of strings of extension(s).
#' 
#' @keywords internal
pmx_make_extension <- function(x) {
  dplyr::if_else(!stringr::str_detect(x, '^\\..+'), stringr::str_c('.', x), x)
}

#' Update file extension
#' 
#' @description Change the extension of a file.
#' 
#' @param x A string or vector of strings containing the file name to be modified.
#' @param ext A string or vector of strings containing the name of the new extension(s).
#' 
#' @return A string or vector of strings of file name(s).
#' 
#' @keywords internal
pmx_update_extension <- function(x, ext) {
  stringr::str_replace(x, '\\.[[:alnum:]]+$', ext)
}

#' Message function
#' 
#' @description Message function with quiet option inspired from `ronkeizer/vpc`.
#' 
#' @param txt A string for the message.
#' @param quiet Should messages be displayed to the console.
#' 
#' @return Silent when quiet is `TRUE` or a message is quiet is `FALSE`.
#' 
#' @keywords internal
pmx_msg <- function(txt, quiet = TRUE) {
  if (!quiet) message(txt)
}


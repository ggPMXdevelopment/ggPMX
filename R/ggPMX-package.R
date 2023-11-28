#' ggPMX: A ggplot2 toolbox for Nonlinear Mixed-Effect Model graphical
#'
#' This package aims to generate diagnostic plots in a standard way. The tool reads data
#' from many sources (MONOLIX,NONMEM,others) and generates standard grahics
#' that can be easily integrated in a single report.
#'
#' \itemize{
#' \item Get data from different system and create a data source
#' \item Plot many plots using the generic plot method \code{\link{plot_pmx}}.
#' }
#'
#' For support, feedback or bug reports, please reach out to
#' \email{ggPMX_ORG@@dl.mgd.novartis.com}.
#'
#' @section Version History:
#'
#' \describe{
#'
#' \item{Jan 11 2017, 0.0.0}{Init ggPMX from Novartis rtemplate.}
#' \item{Feb 06 2017, 0.3.0}{Import version 0.3.0 of package.}
#'
#' }
#'
#' @name ggPMX
#' @docType package
#' @importFrom R6 R6Class
#' @import ggplot2
#' @importFrom gtable gtable_add_cols gtable_add_rows
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim
#' @importFrom utils read.table assignInMyNamespace
#' @importFrom yaml yaml.load_file
#' @import data.table
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @keywords internal
"_PACKAGE"
NULL

# Included dataset documentation example
# ----------------------------------------
#
# DELETE THIS FILE FROM YOUR PACKAGE IF THE EXAMPLE IS NO LONGER NEEDED.
#
# The following is an example how data sets that are included in your package
# should be documented. It contains the documentation for the data set that is 
# derived by the code in tools/make-ds.R and the data-raw/testdata.csv input
# data set while the package is built.

#' Example data set.
#'
#' Data set containing some random data.
#'
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{col1}{Some character valued data (values "X", "y", "z")}
#'   \item{col2}{Some integer numbers}
#'   \item{col3}{Some draws from a standard-normal distribution}
#' }
#' @references Beaten D. et. al, \emph{The Lancet}, 2013, (382), 9906, p 1705
#' 
#' @family example items
"testdata"

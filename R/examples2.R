# Code and object documentation example, part 2
# ---------------------------------------------
#
# DELETE THIS FILE FROM YOUR PACKAGE IF THE EXAMPLE IS NO LONGER NEEDED.

#' Return numbers from example data.
#'
#' Retrieve data.
#'
#' @param index Index of number to be returned (1 - 5).
#' @param dataset Index of data set to be used (default = 1). Must be 1 or 2.
#'
#' @details This is function is an example to demonstrate the use of internal
#' data sets.
#'
#' @return The requested data item.
#'
#' @family example items
#'
#' @examples
#' dat21 <- retrieve_data(2)
#' dat52 <- retrieve_data(5, 2)
#'
#' @export
retrieve_data <- function(index, dataset=1) {

    assert_number(index, lower=1, upper=5)
    assert_number(dataset, lower=1, upper=2)
    
    if (dataset == 1){
        return(dat1[index, "col2"])
    } else {
        return(dat2[index, "col2"])
    }
}

# Code and object documentation example, part 1
# ---------------------------------------------
#
# DELETE THIS FILE FROM YOUR PACKAGE IF THE EXAMPLE IS NO LONGER NEEDED.
#
# This file contains two example functions. See the comments in the @details
# documentation items below for more details.

#' Add two numbers.
#'
#' Arithmetic addition of two numbers.
#'
#' @param x A number.
#' @param y A number.
#'
#' @return The sum of \code{x} and \code{y}.
#'
#' @details This is function is meant to be a simple example, how a
#' function in your code should be documented, including a few tricks,
#' like:
#'
#' - Inclusion of repeated documentation chunks (template): have a look into
#' /man-roxygen/example_repeatdocu.R for the implementation
#'
#' - Referencing of related functionality (seealso, family)
#'
#' @template example_repeatdocu
#'
#' @seealso \code{\link{subtract}}
#' @family example items
#'
#' @examples
#' add1 <- add(1, 1)
#' add2 <- add(10, 1)
#'
#' @export
add <- function(x, y) {
    assert_number(x)
    assert_number(y)
    return(x + y)
}


#' Subtract two numbers.
#'
#' Arithmetic difference of two numbers.
#'
#' @param x A number to subtract from.
#' @param y A number to subtract.
#'
#' @details This is function is meant to be another simple example
#' which calculates \eqn{x - y}.
#'
#' @return The difference of \code{x} and \code{y}.
#'
#' @seealso \code{\link{add}}
#' @family example items
#'
#' @examples
#' subtract1 <- subtract(1, 1)
#' subtract2 <- subtract(10, -1)
#'
#' @export
subtract <- function(x, y) {
    assert_number(x)
    assert_number(y)
    return(x - y)
}



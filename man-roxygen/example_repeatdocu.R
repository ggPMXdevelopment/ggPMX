# Repeated chunks of documentation example
# ----------------------------------------
#
# DELETE THIS FILE FROM YOUR PACKAGE IF THE EXAMPLE IS NO LONGER NEEDED.
#
# The following is an example how chunks of documentation that need to be 
# included in the documentation of multiple parts of your package can be
# coded centrally. See the "add" function defined in R/examples.R for an example 
# how to make the chunk appear in the documentation of the function, using
# #' @template example_repeatdocu

#' @section Repeated chunks of documentation example:
#' 
#' \tabular{lccc}{
#' \strong{Prior/Posterior} \tab \strong{Likelihood} \tab \strong{Predictive} 
#'  \tab \strong{Summaries} \cr
#' Beta \tab Binomial \tab Beta-Binomial \tab \code{n}, \code{r} \cr
#' Normal \tab Normal (\emph{fixed \eqn{\sigma}}) \tab Normal \tab \code{n}, \code{m}, \code{s}  \cr
#' Gamma \tab Poisson \tab Gamma-Poisson \tab  \code{n}, \code{m} \cr
#' Gamma \tab Exp \tab Gamma-Exp (\emph{missing}) \tab \code{n}, \code{m}
#' }
#' 

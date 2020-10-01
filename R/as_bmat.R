# This code is from the mrgsolve package (matrix.R), used for read_extfile (Thanks to Kyle T Baron)

##' Coerce R objects to block or diagonal matrices
##' 
##' These are simple functions that may be helpful to create the matrix objects
##' that mrgsolve expects.  Functions are named based on whether they create a
##' diagonal matrix (\code{d}), a block matrix (\code{b}), or a a correlation
##' matrix (\code{c}).
##' 
##'
##' @param x data frame or list 
##' @param pat regular expression, character
##' @param cols column names to use instead of \code{pat}
##' @param ... arguments passed to \code{\link{dmat}} or \code{\link{bmat}}
##' @return A numeric matrix for list and numeric methods.  For data.frames, 
##' a list of matrices are returned.
##' @seealso \code{\link{bmat}}, \code{\link{dmat}}, \code{\link{cmat}}
##' 
##' @details
##' Use \code{as_dmat} to create a diagonal matrix, \code{as_bmat}
##' to create a block matrix, and \code{as_cmat} to create a block 
##' matrix where diagonal elements are understood to be correlations
##' rather than covariances. \code{as_cmat} uses \code{as_bmat} to 
##' form the matrix and then converts off-diagonal elements to 
##' covariances before returning.
##' 
##' The methods for \code{data.frame} will work down the rows
##' of the data frame and make the appropriate matrix from 
##' the data in each row.  The result is a list of matrices. 
##' 
##' @examples
##'
##' df <- data.frame(
##'   OMEGA1.1 = c(1,2),
##'   OMEGA2.1 = c(11,22),
##'   OMEGA2.2 = c(3,4),
##'   SIGMA1.1 = 1,
##'   FOO=-1
##' )
##'
##' as_bmat(df, "OMEGA")
##' as_dmat(df,"SIGMA")
##' as_dmat(df[1,],"OMEGA")
##'
##' @rdname matrix_converters
##' 
##' @export
setGeneric("as_bmat", function(x,...) standardGeneric("as_bmat"))

##' @rdname matrix_converters
##' @export
setMethod("as_bmat", "list", function(x,...) {
  as_bmat(unlist(x),...)
})

##' @rdname matrix_converters
##' @export
setMethod("as_bmat", "numeric", function(x,pat="*",...) {
  x <- grepn(x,pat, !missing(pat))
  do.call("bmat", list(x,...))
})

##' @rdname matrix_converters
##' @export
setMethod("as_bmat", "data.frame", function(x,pat="*",cols=NULL, ...) {
  if(is.character(cols)) {
    cols <- cvec_cs(cols)
    if(!all(cols %in% names(x))) {
      stop("Invalid colums in cols argument.") 
    }
    cols <- names(x) %in% cols
  } else {
    cols <- grepl(pat,names(x))
  }
  x <- x[,cols,drop=FALSE]
  lapply(seq_len(nrow(x)), function(i) bmat(unlist(x[i,],use.names=FALSE),...))
})

##' @rdname matrix_converters
##' @export
setMethod("as_bmat", "ANY", function(x,...) {
  as_bmat(as.data.frame(x),...)
})


# Utils
grepn <- function(x,pat,warn=FALSE) {
  if(is.null(names(x))) {
    if(warn) {
      warning("grepn: pattern was specified, but names are NULL.", 
              call.=FALSE)
    }
    return(x)
  }
  if(pat=="*") return(x)
  x[grepl(pat,names(x),perl=TRUE)]
}

##' Create a square numeric matrix from the lower-triangular elements
##'
##' @param x numeric data
##' @param context the working context
##' @return a square symmetric numeric matrix with column names
##' @keywords internal
lower2matrix <- function(x, context=NULL) {
  x <- as.numeric(x)
  if(length(x)==1) return(matrix(x,nrow=1, ncol=1 ))
  n <- 0.5*(sqrt(1-4*(-2*length(x)))-1)
  if(!n==as.integer(n)) {
    stop(paste0("Block matrix has invalid specification (", context, ")."),call.=FALSE)
  }
  mat <- diag(n)
  mat[upper.tri(mat,diag=TRUE)] <- x
  mat <- mat+t(mat) - diag(diag(mat))
  mat
}

##' Create matrices from vector input
##'
##' @param ... matrix data
##' @param correlation logical; if TRUE, off diagonal elements are assumed 
##' to be correlations and converted to covariances
##' @param digits if greater than zero, matrix is passed to signif (along 
##' with digits) prior to returning
##' @details
##' \code{bmat} makes a block matrix.  \code{cmat} makes a correlation matrix. 
##' \code{dmat} makes a diagonal matrix.
##' 
##' @seealso \code{\link{as_bmat}}
##' 
##' @examples
##'
##' dmat(1,2,3)/10
##'
##' bmat(0.5,0.01,0.2)
##'
##' cmat(0.5, 0.87,0.2)
##' 
##' @rdname matrix_helpers
##' @name matrix_helpers
##' @export
bmat <- function(...,correlation=FALSE, digits=-1) {
  x <- lower2matrix(unlist(list(...)),context="bmat")
  if(correlation) decorr(x)
  if(digits>0) x <- signif(x,digits=digits)
  return(x)
}

##' @rdname matrix_helpers
##' @export
cmat <- function(...,digits=-1) {
  bmat(...,digits=digits,correlation=TRUE)
}

#Lifted from https://github.com/hadley/plyr/blob/main/R/quote.r

#' Convert input to quoted variables.
#'
#' Convert characters, formulas and calls to quoted .variables
#'
#' This method was taken from `plyr` so it wouldn't be an additional dependency
#'
#' Currently conversions exist for character vectors, formulas and
#' call objects.
#'
#' @return a list of quoted variables
#' @seealso \code{\link[=quoted]{.}}
#' @param x input to quote
#' @param env environment in which unbound symbols in expression should be
#'   evaluated. Defaults to the environment in which \code{ggpmx_quoted} was
#'   executed.
#' @export
#' @examples
#' ggpmx_quoted(c("a", "b", "log(d)"))
#' ggpmx_quoted(a ~ b + log(d))
ggpmx_quoted <- function(x, env = parent.frame()) UseMethod("ggpmx_quoted")

#' @export
ggpmx_quoted.call <- function(x, env = parent.frame()) {
  structure(as.list(x)[-1], env = env, class = "quoted")
}

#' @export
ggpmx_quoted.character <- function(x, env = parent.frame()) {
  structure(
    lapply(x, function(x) parse(text = x)[[1]]),
    env = env, class = "quoted"
  )
}

#' @export
ggpmx_quoted.numeric <- function(x, env = parent.frame()) {
  structure(x, env = env, class = c("quoted", "numeric"))
}

#' @export
ggpmx_quoted.formula <- function(x, env = parent.frame()) {
  simplify <- function(x) {
    if (length(x) == 2 && x[[1]] == as.name("~")) {
      return(simplify(x[[2]]))
    }
    if (length(x) < 3) return(list(x))
    op <- x[[1]]; a <- x[[2]]; b <- x[[3]]

    if (op == as.name("+") || op == as.name("*") || op == as.name("~")) {
      c(simplify(a), simplify(b))
    } else if (op == as.name("-")) {
      c(simplify(a), bquote(-.(x), list(x=simplify(b))))
    } else {
      list(x)
    }
  }

  structure(simplify(x), env = env, class = "quoted")
}

#' @export
ggpmx_quoted.quoted <- function(x, env = parent.frame()) x

#' @export
ggpmx_quoted.NULL <- function(x, env = parent.frame()) {
  structure(list(), env = env, class = "quoted")
}

#' @export
ggpmx_quoted.name <- function(x, env = parent.frame()) {
  structure(list(x), env = env, class = "quoted")
}

#' @export
ggpmx_quoted.factor <- function(x, env = parent.frame()) {
  ggpmx_quoted(as.character(x), env)
}


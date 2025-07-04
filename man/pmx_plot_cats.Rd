% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmx-all-plots.R
\name{pmx_plot_cats}
\alias{pmx_plot_cats}
\title{Generic pmx stratified plot}
\usage{
pmx_plot_cats(ctr, pname, cats, chunk = "", print = TRUE, ...)
}
\arguments{
\item{ctr}{\code{pmxClass} pmx controller}

\item{pname}{plot name}

\item{cats}{list of categorical variables. By default all of them}

\item{chunk}{chunk name}

\item{print}{\code{logical} if TRUE print plots otherwise the list of plots is returned}

\item{...}{others graphics parameters passed :
\itemize{
\item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
\item \code{\link{pmx_qq}} quantile-quantile plot object
\item \code{\link{pmx_update}} function.
}}
}
\value{
ggplot2 object(s)
}
\description{
Generic pmx stratified plot
}

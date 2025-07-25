% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmx-all-plots.R
\name{pmx_plot}
\alias{pmx_plot}
\title{Generic pmx plot}
\usage{
pmx_plot(ctr, pname, ...)
}
\arguments{
\item{ctr}{\code{pmxClass} pmx controller}

\item{pname}{plot name}

\item{...}{others graphics parameters passed :
\itemize{
\item \code{\link{pmx_gpar}} internal function to customize shared graphical parameters
\item \code{\link{pmx_qq}} quantile-quantile plot object
\item \code{\link{pmx_update}} function.
}}
}
\value{
ggplot2 object
}
\description{
Generic pmx plot
}

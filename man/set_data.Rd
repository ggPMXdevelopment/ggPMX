% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmxClass.R
\name{set_data}
\alias{set_data}
\title{Set a controller data set}
\usage{
set_data(ctr, ..., envir = parent.frame())
}
\arguments{
\item{ctr}{the controller object}

\item{...}{a named  list parameters (see example)}

\item{envir}{the \code{\link[base]{environment}} in which \code{expr} is to
    be evaluated.  May also be \code{NULL}, a list, a data frame,
    a pairlist or an integer as specified to \code{\link[base]{sys.call}}.}
}
\description{
Set a controller data set
}
\details{
This function can be used to set an existing data set or to create a new one. The basic
idea is to change the  built-in data set (change the factor level names, change some rows
values or apply any other data set operation) and use the new data set using the dname
parameter of pmx_plot family functions.
}
\examples{
\donttest{
ctr <- theophylline()
dx <- ctr \%>\% get_data("eta")
dx <- dx[, EFFECT := factor(
  EFFECT,
  levels = c("ka", "V", "Cl"),
  labels = c("Concentration", "Volume", "Clearance")
)]
## update existing data set
ctr \%>\% set_data(eta = dx)
## or create a new data set
ctr \%>\% set_data(eta_long = dx)
}
}
\seealso{
Other pmxclass: 
\code{\link{get_cats}()},
\code{\link{get_conts}()},
\code{\link{get_covariates}()},
\code{\link{get_data}()},
\code{\link{get_occ}()},
\code{\link{get_plot}()},
\code{\link{get_plot_config}()},
\code{\link{get_strats}()},
\code{\link{plot_names}()},
\code{\link{plots}()},
\code{\link{pmx_update}()},
\code{\link{set_plot}()}
}
\concept{pmxclass}

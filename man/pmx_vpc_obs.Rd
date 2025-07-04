% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-vpc.R
\name{pmx_vpc_obs}
\alias{pmx_vpc_obs}
\title{Sets vpc observation layer}
\usage{
pmx_vpc_obs(show = TRUE, color = "#000000", size = 1, alpha = 0.7, shape = 1)
}
\arguments{
\item{show}{\code{logical} if TRUE show observation points}

\item{color}{\code{character} Color of the observed endpoint values. Default: "#000000".}

\item{size}{\code{numeric} Size of the observed endpoint values. Default: 1.}

\item{alpha}{\code{numeric} Transparency of the observed endpoint values. Default: 0.7.}

\item{shape}{\code{numeric} Shape of the observed endpoint values. Default: 1.}
}
\value{
list with options for ggplot2 layer with observations
}
\description{
Sets vpc observation layer
}
\seealso{
Other vpc: 
\code{\link{pmx_plot_vpc}()},
\code{\link{pmx_vpc}()},
\code{\link{pmx_vpc_bin}()},
\code{\link{pmx_vpc_ci}()},
\code{\link{pmx_vpc_pi}()},
\code{\link{pmx_vpc_rug}()}
}
\concept{vpc}

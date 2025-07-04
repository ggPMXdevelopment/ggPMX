% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-vpc.R
\name{pmx_vpc_pi}
\alias{pmx_vpc_pi}
\title{Sets vpc percentile layer}
\usage{
pmx_vpc_pi(
  show = c("all", "median", "area"),
  interval = c(0.05, 0.95),
  median = list(color = "#000000", linewidth = 1, alpha = 0.7, linetype = "solid"),
  extreme = list(color = "#000000", linewidth = 1, alpha = 0.7, linetype = "dashed"),
  area = list(fill = "blue", alpha = 0.1)
)
}
\arguments{
\item{show}{\code{charcater} how lines are displayed:
\itemize{
\item \strong{show=all} lines will be displayed for each of
the 3 percentiles. with a shaded area.
\item \strong{show=median} Show only median line.
\item \strong{show=area} Show only median line and the shaded area
}}

\item{interval}{\code{numeric} quantiles values default
to \code{c(.05,.95)}}

\item{median}{\code{list} containing: \cr
\itemize{
\item \strong{color} \code{charcater}  Color of the median percentile line. Default: "#000000".
\item \strong{linewidth} \code{numeric}  Thickness of the median percentile line. Default: 1.
\item \strong{alpha} \code{numeric} Transparency of the median percentile line. Default: 0.7.
\item \strong{linetype} \code{charcater} Linetype of the median percentile line. Default: "solid".
}}

\item{extreme}{\code{list} containing: \cr
\itemize{
\item \strong{color} \code{charcater} Color of the median percentile line. Default: "#000000".
\item \strong{linewidth} \code{numeric} Thickness of the median percentile line. Default: 1.
\item \strong{alpha} \code{numeric} Transparency of the median percentile line. Default: 0.7.
\item \strong{linetype} \code{charcater} Linetype of the median percentile line. Default: "solid"
}}

\item{area}{\code{list} containing: \cr
\itemize{
\item \strong{fill} \code{charcater}  Color of the shaded area. Default: "blue".
\item \strong{alpha} \code{numeric} Transparency of the shaded area. Default: 0.1.
}}
}
\value{
list with options for Prediction Interval layer
}
\description{
Sets vpc percentile layer
}
\seealso{
Other vpc: 
\code{\link{pmx_plot_vpc}()},
\code{\link{pmx_vpc}()},
\code{\link{pmx_vpc_bin}()},
\code{\link{pmx_vpc_ci}()},
\code{\link{pmx_vpc_obs}()},
\code{\link{pmx_vpc_rug}()}
}
\concept{vpc}

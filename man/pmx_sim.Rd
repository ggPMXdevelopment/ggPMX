% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmxClass.R
\name{pmx_sim}
\alias{pmx_sim}
\title{Create simulation object}
\usage{
pmx_sim(file, data, irun, idv)
}
\arguments{
\item{file}{\code{character} path to the simulation file}

\item{data}{\code{data.table} simulation data}

\item{irun}{\code{character} name of the simulation column}

\item{idv}{\code{character} name of the ind. variable}
}
\description{
Create simulation object
}
\examples{
\donttest{
library(ggPMX)

theo_path <- file.path(
  system.file(package = "ggPMX"), "testdata",
  "theophylline"
)
WORK_DIR <- file.path(theo_path, "Monolix")
input_file <- file.path(theo_path, "data_pk.csv")
vpc_file <- file.path(theo_path, "sim.csv")

ctr <- pmx_mlx(
  config = "standing",
  directory = WORK_DIR,
  input = input_file,
  dv = "Y",
  dvid = "dvid",
  cats = c("SEX"),
  conts = c("WT0", "AGE0"),
  strats = "STUD",
  settings = pmx_settings(
    use.labels=TRUE,
    cats.labels=list(
      SEX=c("0"="Male","1"="Female")
    )
  ),
  sim = pmx_sim(
    file = vpc_file,
    irun ="rep",
    idv="TIME"
  )
)


ctr \%>\% pmx_plot_vpc(
  strat.facet=~SEX,
  facets=list(nrow=2),
  type="percentile",
  is.draft = FALSE,
  pi = pmx_vpc_pi(interval = c(0.1,0.9),
              median=list(color="green"),
              extreme= list(color="green")),
  obs = pmx_vpc_obs(color="blue",shape=18,size=2),
  ci = pmx_vpc_ci(interval = c(0.1,0.9),
              median=list(fill="pink")),
  bin=pmx_vpc_bin("kmeans",n=5)
)

ctr \%>\% 
  pmx_plot_vpc(bin= pmx_vpc_bin(
     style = "fixed",
     fixedBreaks=c(-10,2, 5, 10,15,50))
  )

# example with legend 

ctr \%>\% pmx_plot_vpc(
  is.legend = TRUE,
  pi = pmx_vpc_pi(interval=c(0.02,0.98),median = list(linetype="dotted")),
  ci = pmx_vpc_ci(interval = c(0.05,0.95),median=list(fill="red"))
)

}
}

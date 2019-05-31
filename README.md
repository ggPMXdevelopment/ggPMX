# ggPMX R package


[![Travis-CI Build Status](https://travis-ci.org/ggPMXdevelopment/ggPMX.svg?branch=master)](https://travis-ci.org/ggPMXdevelopment/ggPMX)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ggPMX)](https://cran.r-project.org/package=ggPMX)


ggPMX is an open-source R package freely available on CRAN since April 2019. It generates standard diagnostic plots for mixed effect models used in pharmacometric activities. The package builds on the R-package ggplot2 and aims at providing a workflow that is **consistent**, **reproducible** and **efficient**, resulting in **high quality graphics** ready-to-use in submission documents and publications. Intuitive functions and options allow for optimal figure customization and graphics stratification. ggPMX enables straightforward generation of PDF, Word or PNG output files that contain all diagnostic plots for keeping track of modeling results. The package is currently compatible with Monolix versions 2016 and 2018R1.

Using simple syntax, the toolbox produces various goodness-of-fit diagnostics such as:
- residual- and empirical Bayes estimate (EBE)-based plots, 
- distribution plots, 
- prediction- and simulation-based diagnostics (visual predictive checks). 

In addition, shrinkage and summary parameters tables can be also produced. By default, the PDF- or Word-format diagnostic report contains essential goodness-of-fit plots. However, these can be adapted to produce different sets of diagnostics as desired by the user, and any of the plots may be customized individually. The types of supported customizations include modifications of the graphical parameters, labels, and various stratifications by covariates.

## Documentation

[Vignette](https://github.com/ggPMXdevelopment/ggPMX/blob/master/ggPMX-guide_2019-04-25.pdf)

## Install ggPMX


### CRAN:

```R
install.packages("ggPMX")
```
### Github version:

```R
devtools::install_github("ggPMXdevelopment/ggPMX")
```

## Feedback

ggPMX is now ready for inputs and enhancements by the pharmacometric community.
- Please use [ package issues](https://github.com/ggPMXdevelopment/ggPMX/issues) to fill in your feedback.



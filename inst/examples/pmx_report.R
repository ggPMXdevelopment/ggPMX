
\donttest{
library(ggPMX)
## list of templates
## ctr %>% pmx_report_template()

report_dir <- tempdir()
## case1: generate a single report
## We use default save dir, 
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = report_dir,
  format="report")

## case2: generate standalone plots
## Note here the use of a custom dir to save results
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = report_dir,
  format="plots")


## case3: generate both : reports + plots
## by default add footnote
## Note , you can force footnote to FALSE using footnote parameter
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = report_dir,
  format="both")

## case4 : generate standalone plots with footnotes
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = report_dir,
  footnote=TRUE,
  format="plots")





##  case6: dynamic edit
## uncomment to run 
# ctr <- theophylline()
# ctr %>% pmx_report(
#   save_dir = file.path(report_dir,"case6"),
#   name = "my_report",
#   format="report",
#   edit = TRUE)


## case7 : use custom template file 

ctr <- theophylline()
custom_template <- 
  file.path( system.file(package = "ggPMX"),"examples","templates","custom_report.Rmd")
ctr %>% pmx_report(
  name="report2",
  save_dir = report_dir,
  template=custom_template,
  format="both"
)

## case7 : generate individual plots report 

## ctr <- theophylline()
## ctr %>% pmx_report(
##   name="report2",
##   save_dir = report_dir,
##   template="individual",
##   format="both",
##   which_pages=1:2
## )

## case8: misc example with complicated features
## see github issue : #179
ctr <- theophylline()
misc_template <- 
  file.path( system.file(package = "ggPMX"),"examples","templates","misc.Rmd")
ctr %>% pmx_report(
  name="misc",
  save_dir = report_dir,
  template=misc_template,
  format="both"
)
}


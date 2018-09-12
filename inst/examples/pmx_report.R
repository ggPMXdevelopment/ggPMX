library(ggPMX)



## list of templates
## ctr %>% pmx_report_template()

report_dir <- path.expand("~/report_dir")
## case1: generate a single reprot
## We use default save dir, 
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = file.path(report_dir,"case1"),
  format="report")

## case2: generate standalone plots
## Note here the use of a custom dir to save results
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = file.path(report_dir,"case2"),
  format="plots")


## case3: generate both : reports + plots
## by default add footnote
## Note , you can force footnote to FALSE using footnote parameter
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = file.path(report_dir,"case3"),
  format="both")

## case4 : generate standalone plots with footnotes
ctr <- theophylline()
ctr %>% pmx_report(
  name = "my_report",
  save_dir = file.path(report_dir,"case4"),
  footnote=TRUE,
  format="plots")


##  case5: use a custom template 
ctr <- theophylline(settings = pmx_settings(use.titles = FALSE))
ctr %>% pmx_report(
      name = "my_report",
      save_dir = file.path(report_dir,"case5"),
      template="standing_with_header",
      format="both")


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
  save_dir =  file.path(report_dir,"case1"),
  template=custom_template,
  format="both"
)

## case7 : generate individual plots report 

ctr <- theophylline()
ctr %>% pmx_report(
  name="report2",
  save_dir =  file.path(report_dir,"case1"),
  template="individual",
  format="both",
  npage=1:2
)

## case8: misc example with complicated features
## see github issue : #179
ctr <- theophylline()
misc_template <- 
  file.path( system.file(package = "ggPMX"),"examples","templates","misc.Rmd")
ctr %>% pmx_report(
  name="misc",
  save_dir =  file.path(report_dir,"misc"),
  template=misc_template,
  format="both"
)

\donttest{

library(ggPMX)
# you probably want to create the report in your own directory
# But using a temp directory allows for easy cleanup

## case1: generate a single report
withr::with_tempdir({

  ctr <- theophylline()
  ctr %>% pmx_report(
    name = "my_report",
    save_dir = getwd(),
    format="report")
})

## case2: generate standalone plots
withr::with_tempdir({
  ctr <- theophylline()
  ctr %>% pmx_report(
    name = "my_report",
    save_dir = getwd(),
    format="plots")
})

## case3: generate both : reports + plots
## by default add footnote
## Note, you can force footnote to FALSE using footnote parameter
withr::with_tempdir({

  ctr <- theophylline()
  ctr %>% pmx_report(
    name = "my_report",
    save_dir = getwd(),
    format="both")

})





## case4 : generate standalone plots with footnotes
withr::with_tempdir({

  ctr <- theophylline()
  ctr %>% pmx_report(
    name = "my_report",
    save_dir = getwd(),
    footnote=TRUE,
    format="plots")

})





##  case6: dynamic edit
## uncomment to run
# ctr <- theophylline()
# ctr %>% pmx_report(
#   save_dir = file.path(getwd(),"case6"),
#   name = "my_report",
#   format="report",
#   edit = TRUE)


## case7 : generate individual plots report
## ctr <- theophylline()
## ctr %>% pmx_report(
##   name="report2",
##   save_dir = getwd(),
##   template="individual",
##   format="both",
##   which_pages=1:2
## )

}


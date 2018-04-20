library(ggPMX)

ctr <- theophylline(input_file = "~/report_dir/case1/theophylline.csv",
                    input=

## list of templates
## ctr %>% pmx_report_template()


## case1: generate a single reprot
## We use default save dir, 
ctr <- theophylline(input_file = "~/report_dir/case1/theophylline.csv")
ctr %>% pmx_report(
  name = "my_report",
  output_type="report")

## case2: generate standalone plots
## Note here the use of a custom dir to save results
ctr <- theophylline(input_file = "~/report_dir/case1/theophylline.csv")
ctr %>% pmx_report(
  name = "my_report",
  save_dir = "~/report_dir/case2",
  output_type="plots")


## case3: generate both : reports + plots
## by default add footnote, set footnote to FALSE in case you to get rid of it
ctr <- theophylline(input_file = "~/report_dir/case1/theophylline.csv")
ctr %>% pmx_report(
  name = "my_report",
  save_dir = "~/report_dir/case3",
  output_type="both")

## case4 : generate standalone plots with footnotes
ctr <- theophylline(input_file = "~/report_dir/case1/theophylline.csv")
ctr %>% pmx_report(
  name = "my_report",
  save_dir = "~/report_dir/case4",
  footnote=TRUE,
  output_type="plots")


##  case5: use a custom template 
ctr <- theophylline(input_file = "~/report_dir/case1/theophylline.csv",
                    settings = pmx_settings(use.titles = FALSE))
ctr %>% pmx_report(
      name = "my_report",
      save_dir = "~/report_dir/case5",
      template="standing_with_header",
      output_type="both")


##  case6: dynamic edit
ctr <- theophylline(input_file = "~/report_dir/case1/theophylline.csv")
ctr %>% pmx_report(
  save_dir = "~/report_dir/case6",
  name = "my_report",
  output_type="report",
  edit = TRUE)


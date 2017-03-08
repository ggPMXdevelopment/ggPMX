context("monolix_outputs")

monolix_data <- source_monolix_data()

for(i in 1:length(monolix_data[["PK"]])){
  # define ctr
  testthat::test_that(
    paste0("canSetControllerFor", names(monolix_data[["PK"]][i])), {
      ctr <- pmx(config = "standing", sys = "mlx", 
                 directory = monolix_data[["PK"]][[i]])
    })
  
  testthat::test_that(
    paste0("canPlotINDfor", names(monolix_data[["PK"]][i])), 
    {
      plotName <- paste0("testIND", names(monolix_data[["PK"]][i]))
      ctr %>% set_plot(ptype="IND", pname = plotName)
      ctr %>% get_plot(plotName)
    }
  )
  
}

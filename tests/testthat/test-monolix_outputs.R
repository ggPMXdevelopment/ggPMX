context("monolix_outputs")

monolix_data <- source_monolix_data()

for(i in 1:length(monolix_data[["PK"]])){
  pmxOptions(work_dir = monolix_data[["PK"]][[i]])
  # define ctr
  testthat::test_that(
    paste0("canSetControllerFor", names(monolix_data[["PK"]][i])), {
      ctr <- pmx_mlx("standing")
    })
  
  testthat::test_that(
    paste0("canPlotINDfor", names(monolix_data[["PK"]][i])), 
    {
      ctr <- pmx_mlx("standing")
      plotName <- paste0("testIND", names(monolix_data[["PK"]][i]))
      ctr %>% set_plot(ptype="IND", pname = plotName)
      ctr %>% get_plot(plotName)
    }
  )

  testthat::test_that(
    paste0("canPlotDISfor", names(monolix_data[["PK"]][i])), 
    {
      ctr <- pmx_mlx("standing")
      plotName <- paste0("testDIS", names(monolix_data[["PK"]][i]))
      ctr %>% set_plot(ptype="DIS", pname = plotName)
      ctr %>% get_plot(plotName)
    }
  )

  testthat::test_that(
    paste0("canPlotRESfor", names(monolix_data[["PK"]][i])), 
    {
      ctr <- pmx(config = "standing", sys = "mlx", 
                 directory = monolix_data[["PK"]][[i]])
      plotName <- paste0("testRES", names(monolix_data[["PK"]][i]))
      ctr %>% set_plot(ptype="RES", pname = plotName, x = )
      ctr %>% get_plot(plotName)
    }
  )
  
}

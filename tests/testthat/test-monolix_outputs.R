context("monolix_outputs")

monolix_data <- source_monolix_data()

for(i in 1:length(monolix_data[["PK"]])){
  pmxOptions(work_dir = monolix_data[["PK"]][[i]])
  resName <- names(monolix_data[["PK"]][i])
  # define ctr
  testthat::test_that(
    paste0("canSetControllerFor", resName), {
      ctr <- pmx_mlx("standing")
    })
  
  testthat::test_that(
    paste0("canPlotINDfor", resName), 
    {
      ctr <- pmx_mlx("standing")
      plotName <- paste0("testIND", resName)
      ctr %>% set_plot(ptype="IND", pname = plotName)
      ctr %>% get_plot(plotName)
    }
  )

  testthat::test_that(
    paste0("canPlotDISfor", resName), 
    {
      ctr <- pmx_mlx("standing")
      plotName <- paste0("testDIS", resName)
      ctr %>% set_plot(ptype="DIS", pname = plotName)
      ctr %>% get_plot(plotName)
    }
  )

  testthat::test_that(
    paste0("canPlotRESfor", resName), 
    {
      ctr <- pmx_mlx("standing")
      # check plots
      appVars <- names(ctr[["data"]][["mod_pred"]])
      # NPD versus EPRED
      testVars <- c("NPD", "EPRED")
      if(all(testVars %in% appVars)){
        plotName <- paste0("testRESx=", testVars[2], "y=", testVars[1],
                           names(monolix_data[["PK"]][i]))
        ctr %>% set_plot(ptype="RES", pname = plotName, 
                         x = testVars[2], y = testVars[1])
        ctr %>% get_plot(plotName)
      }
      # NPD versus TIME
      testVars <- c("NPD", "TIME")
      if(all(testVars %in% appVars)){
        plotName <- paste0("testRESx=", testVars[2], "y=", testVars[1],
                           names(monolix_data[["PK"]][i]))
        ctr %>% set_plot(ptype="RES", pname = plotName, 
                         x = testVars[2], y = testVars[1])
        ctr %>% get_plot(plotName)
      }
      # IWRES versus IPRED
      testVars <- c("IWRES", "IPRED")
      if(all(testVars %in% appVars)){
        plotName <- paste0("testRESx=", testVars[2], "y=", testVars[1],
                           names(monolix_data[["PK"]][i]))
        ctr %>% set_plot(ptype="RES", pname = plotName, 
                         x = testVars[2], y = testVars[1])
        ctr %>% get_plot(plotName)
      }
      # IWRES versus TIME
      testVars <- c("IWRES", "TIME")
      if(all(testVars %in% appVars)){
        plotName <- paste0("testRESx=", testVars[2], "y=", testVars[1],
                           names(monolix_data[["PK"]][i]))
        ctr %>% set_plot(ptype="RES", pname = plotName, 
                         x = testVars[2], y = testVars[1])
        ctr %>% get_plot(plotName)
      }
    }
  )
  
}

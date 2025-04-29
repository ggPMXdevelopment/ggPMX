skip_if_not(file.exists(test_path("censoring1_project.zip")))
.path <- normalizePath(test_path("censoring1_project.zip"))
withr::with_tempdir({
  
  unzip(.path)
  
  ctr <- suppressWarnings(pmx_mlxtran("censoring1_project.mlxtran"))

  test_that("BLQ column class is correct", {
    # Check if the sim_blq dataset exists
    expect_true("sim_blq_npde_iwres" %in% names(ctr$data), info = "sim_blq dataset is missing in ctr$data.")
    
    # Check if the npde_simBlq column exists in the dataset
    expect_true("npde_simBlq" %in% colnames(ctr$data[["sim_blq"]]),
                info = "npde_simBlq column is missing in sim_blq_npde_iwres dataset.")
    
    # Verify the class of the column
    expect_is(ctr$data[["sim_blq"]][["npde_simBlq"]], "numeric",
              info = "npde_simBlq column is not of class numeric.")
  })
  
  
  test_that("Variable mapped to y-aesthetic is correct", {
    # Generate the plot with sim_blq = TRUE
    plot <- pmx_plot_npde_pred(ctr, sim_blq = TRUE)
    
    # Extract the ggplot mapping
    mapping <- ggplot2::ggplot_build(plot)$data[[1]]
    
    # Check that the y aesthetic is mapped to the correct column
    expect_true("y" %in% names(mapping), info = "y aesthetic is missing in the plot mapping.")
    expect_equal(mapping$y, ctr$data[["sim_blq"]][["npde_simBlq"]],
                 info = "y aesthetic is not correctly mapped to npde_simBlq.")
  })
  
  
  test_that("Exact values of npde_simBlq match the residuals file", {
    # Load the reference data 
    residuals_path <- normalizePath("censoring1_project/ChartsData/ScatterPlotOfTheResiduals/Y_residuals.txt") 
    residuals_file <- fread(residuals_path)
    
    expected_values <- residuals_file[["npde_simBlq"]]
    
    # Check if the npde_simBlq column exists
    expect_true("npde_simBlq" %in% colnames(ctr$data[["sim_blq_npde_iwres"]]),
                info = "npde_simBlq column is missing in sim_blq dataset.")
    
    # Compare values
    actual_values <- ctr$data[["sim_blq_npde_iwres"]][["npde_simBlq"]]
    expect_equal(actual_values, expected_values,
                 tolerance = 1e-6, info = "npde_simBlq values do not match expected values.")
  })
  
  
})
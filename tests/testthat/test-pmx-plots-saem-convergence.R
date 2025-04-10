if (helper_skip()) {
  
  ctr <- theophylline()
  
  test_that("saem convergence plots can be produced", {
    
    p1 <- pmx_plot_saem_convergence(ctr)
    p2 <- ctr$get_plot("saem_convergence")
    
    expect_s3_class(p1, "ggplot")
    expect_s3_class(p2, "ggplot")
    
  })
  
  test_that("saem convergence axis labels can be modified", {
    
    p <- pmx_plot_saem_convergence(ctr, labels = list(x = "it", y = "val"))
    expect_equal(p$labels$x, "it")
    expect_equal(p$labels$y, "val")

  })
  
  test_that("saem convergence plots are passed through pmx-add-plot", {
    
    p <- pmx_plot_saem_convergence(ctr, filter = iteration > 100) # .filter_x()
    expect_equal(min(p$data$iteration), 101L)
    
  })
  
}
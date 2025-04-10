if (helper_skip()) {
  
  ctr <- theophylline()
  
  test_that("saem convergence plots can be produced", {
    
    p1 <- pmx_plot_saem_convergence(ctr)
    p2 <- ctr$get_plot("saem_convergence")
    
    expect_s3_class(p1, "ggplot")
    expect_s3_class(p2, "ggplot")
    
  })
  
  test_that("saem convergence labels can be modified", {
    
    custom_labels <- list(x = "it", y = "val", title = "hi")
    p <- pmx_plot_saem_convergence(ctr, labels = custom_labels)
    
    expect_equal(p$labels$x, custom_labels$x)
    expect_equal(p$labels$y, custom_labels$y)
    expect_equal(p$labels$title, custom_labels$title)
    
  })
  
}
if (helper_skip()) {
  
  ctr <- theophylline()

  geom_names <- function(p) {
    vapply(p$layers, function(l) class(l$geom)[1], "")
  }
  
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
  
  test_that("saem convergence reference line is removable", {
    
    p0 <- pmx_plot_saem_convergence(ctr, is.reference_line = TRUE) 
    p1 <- pmx_plot_saem_convergence(ctr, is.reference_line = FALSE)
    
    expect_true(any(grepl("GeomVline", geom_names(p0))))
    expect_false(any(grepl("GeomVline", geom_names(p1))))

  })
  
  test_that("saem convergence reference line is customisable", {
    
    p <- pmx_plot_saem_convergence(
      ctr, 
      is.reference_line = TRUE,
      reference_line = list(
        colour = "black",
        linetype = "dotted"
      )
    )
    ref <- which(grepl("GeomVline", geom_names(p)))
    
    expect_equal(p$layers[[ref]]$aes_params$colour, "black")
    expect_equal(p$layers[[ref]]$aes_params$linetype, "dotted")
    
  })
  
}

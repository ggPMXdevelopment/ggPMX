function(input, output, session) {
  # The selected directory, if any
  userdirectory <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$mlpath, message = FALSE))
    input$mlpath
  })
  
  # The controller
  observe({
    pmxOptions(work_dir = userdirectory())
    ctr <- pmx_mlx("standing")
  }, priority = 100L)
  
  output$plot <- renderPlot({
    ctr %>% get_plot("indiv")
  }
  )
  
}

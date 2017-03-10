ggData <- function(input, output, session) {
  # The selected directory, if any
  userdirectory <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$mlpath, message = FALSE))
    input$mlpath
  })
  
  # The controller
  ctr <- reactive({
    pmx(config = "standing",sys = "mlx",directory = userdirectory())
  })
  
  # Return the reactive that yields the ggplot
  return(ctr)
}

function(input, output) {
  ggplt <- callModule(ggData, "ggid")
  output$plot <- renderPlot(ggplt() %>% get_plot("indiv"))
}

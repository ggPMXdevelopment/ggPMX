ggData <- function(input, output, session) {
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
}

function(input, output) {
  ggplt <- callModule(ggData, "ggid")
  output$plot <- renderPlot({
    browser()
    ggplt() %>% get_plot("indiv")
  }
  )
}

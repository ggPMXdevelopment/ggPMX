function(input, output, session) {
  # The selected directory, if any
  userdirectory <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$mlpath, message = FALSE))
    input$mlpath
  })
  
  plottype <- reactive({
    validate(need(input$plttype, "Please choose a plot type"))
    input$plttype
  })
  
  # The controller
  observe({
    pmxOptions(work_dir = userdirectory())
    ctr <<- pmx_mlx("standing")
  }, priority = 100L)
  
  # pmx parameters
  # observe({
  #   ctr %>% update_plot(ctr, isolate(plottype()))
  # }, priority = 10L)
  
  output$plottypes <- renderUI({
    userdirectory()
    selectInput("plttype", "Choose a plottype:", 
                as.list(ctr$plots()))
  })
  
  output$plot <- renderPlot({
    validate(need(input$mlpath, "Please choose a dataset"), 
             need(input$plttype, "Please choose a plot type"))
    ctr %>% get_plot(plottype())
  }
  )
}

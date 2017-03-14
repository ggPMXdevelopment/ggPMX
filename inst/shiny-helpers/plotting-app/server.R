function(input, output, session) {
  rv <- reactiveValues(updateplot = 0)
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
  plotpars <- reactive({
    conf <- ctr$get_config(plottype())
    if(input$isdraft){
      conf$gp[["is.draft"]] <- TRUE
      conf$gp[["draft"]]<- list(size = input$isdraftsize, 
                              label = input$isdraftlabel,
                              color = input$isdraftcolor)
    }else{
      conf$gp[["is.draft"]] <- FALSE
    }
    if(!is.null(input$titleLabel)){
      conf$gp$labels[["title"]] <- input$titleLabel
    }
    if(!is.null(input$subTitleLabel)){
      conf$gp$labels[["subtitle"]] <- input$subTitleLabel
    }
    if(!is.null(input$xLabel)){
      conf$gp$labels[["x"]] <- input$xLabel
    }
    if(!is.null(input$yLabel)){
      conf$gp$labels[["y"]] <- input$yLabel
    }
    conf
  })
  
  observeEvent(plotpars(), {
    ctr$remove_plot(isolate(plottype()))
    ctr$add_plot(plotpars(), isolate(plottype()))
    # ctr %>% update_plot(pname = isolate(plottype()), plotpars())
    rv$updateplot <- rv$updateplot + 1
  }, priority = 100L)
  
  output$plottypes <- renderUI({
    userdirectory()
    selectInput("plttype", "Choose a plottype:", 
                as.list(ctr$plots()))
  })
  
  output$plot <- renderPlot({
    validate(need(input$mlpath, "Please choose a dataset"), 
             need(input$plttype, "Please choose a plot type"))
    # introduce depence on plotpars
    rv$updateplot
    nn <- plottype()
    if(nn=="indiv") return(ctr%>%get_plot(nn,c(2,4)))
    ctr %>% get_plot(nn)
  }
  )
  
  output$labels <- renderUI({
    # introduce dependency on input$plttype
    conf <- ctr$get_config(plottype())
    tagList(
      h4("Labels"),
      textInput("titleLabel", "Title", conf$gp$labels["title"]),
      textInput("subTitleLabel", "Subtitle", conf$gp$labels["subtitle"]),
      textInput("xLabel", "xLab", conf$gp$labels["x"]),
      textInput("yLabel", "yLab", conf$gp$labels["y"])
    )
  })
}

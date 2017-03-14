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
    # LABEL OPTIONS
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
    
    # DRAFT OPTIONS
    if(!is.null(input$isdraft)){
      if(input$isdraft){
        conf$gp[["is.draft"]] <- TRUE
        conf$gp[["draft"]]<- list(size = input$isdraftsize, 
                                  label = input$isdraftlabel,
                                  color = input$isdraftcolor)
      }else{
        conf$gp[["is.draft"]] <- FALSE
      }
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
      column(4, offset = 1, 
             textInput("titleLabel", "Title", conf$gp$labels["title"]),
             textInput("subTitleLabel", "Subtitle", conf$gp$labels["subtitle"])
      ),
      column(4, offset = 1, 
             textInput("xLabel", "xLab", conf$gp$labels["x"]),
             textInput("yLabel", "yLab", conf$gp$labels["y"])
      )
    )
  })
  
  output$smooth <- renderUI({
    conf <- ctr$get_config(plottype())
    tagList(
      h4("Has Smooth options"),
      column(4, offset = 1, 
             checkboxInput("hassmooth", label = "Has smooth?", 
                           value = conf$gp$has.smooth)
      ),
      column(4, offset = 1,
             conditionalPanel(
               "input.hassmooth == true",
               checkboxInput("hassmoothse", label = "SE?", 
                             value = conf$gp$smooth$se),
               numericInput("hassmoothlinetype", "Line Type", 
                            value = conf$gp$smooth$linetype),
               numericInput("hassmoothsize", "Size", 
                            value = conf$gp$smooth$size, step = 0.1)
             )
             
      )
    )
  })
  
  output$band <- renderUI({
    conf <- ctr$get_config(plottype())
    tagList(
      h4("Has Band"),
      column(4, offset = 1, 
             checkboxInput("hasband", label = "Has band?", 
                           value = conf$gp$has.band)
      ),
      column(4, offset = 1,
             conditionalPanel(
               "input.hasband == true",
               sliderInput("hasbandband", label = "Band", 
                           step = 1L, value = conf$gp$band$y, 
                           min = -10L, max = 10L),
               numericInput("hasbandlinetype", "Line Type", 
                            value = conf$gp$band$linetype),
               numericInput("hasbandsize", "Size", 
                            value = conf$gp$band$size, step = 0.1)
             )
             
      )
    )
    
  })
  
  output$draft <- renderUI({
    conf <- ctr$get_config(plottype())
    tagList(
      h4("Is Draft"),
      column(4, offset = 1, 
             checkboxInput("isdraft", label = "Is Draft?", 
                           value = conf$gp$is.draft)
      ),
      column(4, offset = 1,
             conditionalPanel(
               "input.isdraft == true",
               numericInput("isdraftsize", "Size", 
                            value = conf$gp$draft$size, step = 1L),
               textInput("isdraftlabel", "Label", conf$gp$draft$label),
               textInput("isdraftcolor", "Color", conf$gp$draft$color)
             )
             
      )
    )
    
  })
  }
  
fluidPage(
  
  # Application title
  titlePanel("ggPMX Plot Tester"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      tagList(
        selectInput("mlpath", "Choose a dataset:", 
                    as.list(monolix_data[["PK"]])), 
        uiOutput("plottypes")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

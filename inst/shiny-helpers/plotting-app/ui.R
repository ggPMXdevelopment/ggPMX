fluidPage(
  
  # Application title
  titlePanel("ggPMX Plot Tester"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      csvFileInput("datafile", "User data (.csv format)")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ggid")
    )
  )
)

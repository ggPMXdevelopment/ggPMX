monolixInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    selectInput(ns("mlpath"), "Choose a dataset:", 
                as.list(monolix_data[["PK"]]))
  )
}

fluidPage(
  
  # Application title
  titlePanel("ggPMX Plot Tester"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      monolixInput("resultsPath")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

fluidPage(
  
  # Application title
  titlePanel("ggPMX Plot Tester"),
  fluidRow(
    column(3,
           tagList(
             selectInput("mlpath", "Choose a dataset:", 
                         as.list(monolix_data[["PK"]])), 
             uiOutput("plottypes")
           )
    ),
    column(9, plotOutput("plot"))
  ), 
  h4("Graphical Options"),
  fluidRow(
    tabsetPanel(
      tabPanel("Labels", uiOutput("labels")),
      tabPanel("Smooth", uiOutput("smooth")),
      tabPanel("Band", uiOutput("band")),
      tabPanel("Draft", uiOutput("draft"))
    )
    
  )
)

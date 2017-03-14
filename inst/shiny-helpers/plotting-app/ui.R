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
    column(9,plotOutput("plot"))
  ), 
  h4("Graphical Options"),
  fluidRow(
    wellPanel(
      column(3,
             h4("Labels"),
             textInput("titleLabel", "Title"),
             textInput("subTitleLabel", "Subtitle"),
             textInput("xLabel", "xLab"),
             textInput("yLabel", "yLab")
      ),
      column(3,
             h4("Has Smooth options"),
             checkboxInput("hassmooth", label = "Has smooth?", 
                           value = FALSE),
             conditionalPanel(
               "input.hassmooth == true",
               checkboxInput("hassmoothse", label = "SE?", 
                             value = FALSE),
               numericInput("hassmoothlinetype", "Line Type", 
                            value = 2L),
               numericInput("hassmoothsize", "Size", 
                            value = 0.5, step = 0.1)
             )
      ),
      column(3,
             selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
             selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
      )
    )
  )
  
)

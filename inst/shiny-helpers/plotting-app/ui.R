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
      column(4, offset = 1,
             selectInput('x', 'X', names(dataset)),
             selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
             selectInput('color', 'Color', c('None', names(dataset)))
      ),
      column(4,
             selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
             selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
      )
    )
  )
  
)

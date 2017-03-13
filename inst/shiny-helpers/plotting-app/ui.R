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
             h4("Diamonds Explorer"),
             sliderInput('sampleSize', 'Sample Size', 
                         min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
                         step=500, round=0),
             br(),
             checkboxInput('jitter', 'Jitter'),
             checkboxInput('smooth', 'Smooth')
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

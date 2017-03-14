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
             h4("Has Band"),
             checkboxInput("hasband", label = "Has band?", 
                           value = FALSE),
             conditionalPanel(
               "input.hasband == true",
               sliderInput("hasbandband", label = "Band", 
                             step = 1L, value = c(-2L, 2L), 
                           min = -10L, max = 10L),
               numericInput("hasbandlinetype", "Line Type", 
                            value = 2L),
               numericInput("hasbandsize", "Size", 
                            value = 0.5, step = 0.1)
             )
      ),
      column(3,
             h4("Is Draft"),
             checkboxInput("isdraft", label = "Is Draft?", 
                           value = FALSE),
             conditionalPanel(
               "input.isdraft == true",
               textInput("hasbandlabel", "Label", "DRAFT"),
               textInput("hasbandcolor", "Color", "grey50")
             )
      )
    )
  )
  
)

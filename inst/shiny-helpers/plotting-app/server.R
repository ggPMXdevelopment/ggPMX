function(input, output) {
  ggplt <- callModule(ggData, "ggid")
  output$plot <- renderPlot(ggplt())
}

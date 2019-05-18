# Simple Slider App
library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", label = "choose num", min = 2, max = 100, value = 22),
  plotOutput("hist")
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    title <- paste0(input$num, " random normal values")
    hist(rnorm(input$num))
  })
}

shinyApp(ui, server)

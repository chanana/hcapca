# two inputs and isolate
library(shiny)

ui <- fluidPage(
  sliderInput(
    inputId = 'slider',
    label = 'slide',
    min = 1,
    max = 100,
    value = 33
  ),
  textInput(
    inputId = 'text',
    label = 'title',
    value = 'title'
  ),
  plotOutput(outputId = 'plot')
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(rnorm(input$slider), main = isolate({input$text}))
  })
}

shinyApp(ui, server)

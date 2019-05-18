# action Button
library(shiny)

ui <- fluidPage(
  sliderInput(inputId = 'slider', label = 'slider', min = 1, max = 100, value = 11),
  actionButton(inputId = 'button', label = 'click me'),
  plotOutput(outputId = 'plot')
)

server <- function(input, output, session) {
  data <- eventReactive(eventExpr = input$button, valueExpr = {
    rnorm(input$slider)
  })
  output$plot <- renderPlot({
    plot(data(), main=as.numeric(input$button))
  })
}

shinyApp(ui, server)

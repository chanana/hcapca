# two outputs
library(shiny)

ui <- fluidPage(
  sliderInput(inputId = 'num', label = 'enter num', min = 2, max = 100, value = 22),
  plotOutput(outputId = 'plot'),
  verbatimTextOutput(outputId = 'vtext')
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(rnorm(input$num))
  })
  output$vtext <- renderPrint({
    max(rnorm(input$num))
    summary(rnorm(input$num))
  })
}

shinyApp(ui, server)

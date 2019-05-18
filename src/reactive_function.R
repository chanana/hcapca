# the reactive() function

library(shiny)

ui <- fluidPage(
  sliderInput(inputId = 'slider', label = 'hey', min = 2, max = 200, value = 33),
  plotOutput(outputId = 'plot'),
  verbatimTextOutput(outputId = 'vtext')
)

server <- function(input, output, session) {
  data <- reactive({
    rnorm(input$slider)
  })
  output$plot <- renderPlot({
    hist(data())
  })
  output$vtext <- renderPrint({
    summary(data())
  })
}

shinyApp(ui, server)

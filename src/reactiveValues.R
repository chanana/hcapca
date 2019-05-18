# reactive Values
library(shiny)

ui <- fluidPage(
  actionButton(inputId = 'norm', label = 'normal'),
  actionButton(inputId = 'unif', label = 'uniform'),
  plotOutput(outputId = 'plot')
)

server <- function(input, output, session) {
  rv <- reactiveValues(data))

  observeEvent(eventExpr = input$norm, handlerExpr = {
    rv$data <- rnorm(100)
  })
  observeEvent(eventExpr = input$unif, handlerExpr = {
    rv$data <- runif(100)
  })
  output$plot <- renderPlot({
    hist(rv$data)
  })
}

shinyApp(ui, server)

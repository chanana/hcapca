# Plots and tables with tab sets using data from R
library(shiny)
library(DT)

ui <- fluidPage(
  h1("hello, World!"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset",label = "Choose a dataset:", choices = ls("package:datasets"), selected = "pressure")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Str",
                 verbatimTextOutput(outputId = "dump")
        ),
        tabPanel(title = "Plot",
                 plotOutput(outputId = "plot")
        ),
        tabPanel(title = "Table",
                 DT::dataTableOutput(outputId = "table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$dump <- renderPrint({
    dataset <- base::get(input$dataset, "package:datasets", inherits = FALSE)
    str(dataset)
  })
  output$plot <- renderPlot({
    dataset <- base::get(input$dataset, "package:datasets", inherits = FALSE)
    plot(dataset)
  })
  output$table <- DT::renderDataTable({
    base::get(input$dataset, "package:datasets", inherits = FALSE)
  })
}

shinyApp(ui, server)

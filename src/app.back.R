## app.R ##
library(shiny)
library(shinydashboard)
library(plotly)
#---- UI ----
ui <- dashboardPage(
  header = dashboardHeader(title = "Basic dashboard"),
  #---- Sidebar content ----
  sidebar = dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Tree",
      tabName = "tree",
      icon = icon("tree")
    ),
    menuItem(
      text = "PCA",
      tabName = "pca",
      icon = icon("desktop")
    )
  )),
  #---- Body Content ----
  body = dashboardBody(tabItems(
    #---- First Tab ----
    tabItem(tabName = "tree",
            fluidRow(box(
              dendroNetworkOutput(outputId = 'dendroNetwork')
              , width = 12
            )),
            fluidRow(box(
              selectInput(
                inputId = "node_to_plot",
                label = "Select Node to View Dendrogram",
                choices = nodeNames,
              )
            )),
            fluidRow(box(
              dendroNetworkOutput(outputId = 'node')
              , width = 12
            ))),
    #---- Second tab ----
    tabItem(tabName = 'pca',
            fluidRow(
              box(title = "Node Selection", status="warning", solidHeader = TRUE, collapsible = TRUE, width = 2.4,
                selectInput(
                  inputId = "node_to_plot",
                  label = "Select Node to View PCA",
                  choices = nodeNames,
                )
              ),
              box(title = "X Axis", status="warning", solidHeader = TRUE, collapsible = TRUE, width = 2.4,
                selectInput(
                  inputId = "x_axis",
                  label = "Select x axis to display",
                  choices = colnames(df.pca$x),
                  selected = "PC1"
                )
              ),
              box(title = "Y Axis", status="warning", solidHeader = TRUE, collapsible = TRUE, width = 2.4,
                selectInput(
                  inputId = "y_axis",
                  label = "Select y axis to display",
                  choices = colnames(df.pca$x),
                  selected = "PC2"
                )
              ),
              box(title = "Points on Loadings", status="warning", solidHeader = TRUE, collapsible = TRUE,width = 2.4,
                numericInput(
                  inputId = "N_points",
                  label = "Points on Loadings Plot",
                  value = 1000,
                  min = 50,
                  max = 10000,
                  step = 50
                )
              ),
              box(status="success", solidHeader = TRUE, collapsible = FALSE, width = 2.4,
                actionButton(inputId = 'make_plots', label = 'Plot!', icon(''))
              )
            ),
            fluidRow(box(
              plotlyOutput("Scores")
            ),
            box(
              plotlyOutput("Loadings")
            )))
  ))
)
)

#---- Server functions ----
server <- function(input, output) {
  #---- reactive hca ----
  dendro <- reactive({
    dend <- master_list[[get_node_position(input$node_to_plot)]]$dend
    dend <- as.hclust(dend)
    return(dend)
  })
  #---- eventReactive scores ----
  scoresData <- eventReactive(
    eventExpr = input$make_plots,
    valueExpr = {
      df <- data.frame(df.pca$x[, c(input$x_axis, input$y_axis)])
      colnames(df) <- c("X", "Y")
      return(df)
  })
  #---- eventReactive loadings ----
  loadingsData <- eventReactive(
    eventExpr = input$make_plots,
    valueExpr = {
      df <- data.frame(df.pca$rotation[, c(input$x_axis, input$y_axis)])
      df <- add_euclidean_distance(df)
      df <-
        select_top_N_points(df = df, N_points = input$N_points)
      df <- remove_euclidean_distance_column(df)
      colnames(df) <- c("X", "Y")
      return(df)
  })
  #---- output Tree ----
  output$dendroNetwork <- renderDendroNetwork({
    dendroNetwork(hc = hc)
  })
  #---- output Node ----
  output$node <- renderDendroNetwork({
    dendroNetwork(hc = dendro())
  })
  #---- output scores ----
  output$Scores <- renderPlotly({
    plot_ly(
      scoresData(),
      x = ~ X,
      y = ~ Y,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 10,
        color = 'rgba(255, 182, 193, 0.9)',
        line = list(color = 'rgba(152, 0, 0, 0.8',
                    width = 2)
      ),
      hoverinfo = 'text',
      text = ~ rownames(scoresData())
    )
  })
  #---- output loadings ----
  output$Loadings <- renderPlotly({
    plot_ly(
      loadingsData(),
      x =  ~ X,
      y =  ~ Y,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 10,
        color = 'rgba(184, 184, 255, 0.9)',
        line = list(color = 'rgb(0, 0, 153, 0.8)',
                    width = 2)
      ),
      hoverinfo = 'text',
      text = ~ rownames(loadingsData())
    )
  })
}

shinyApp(ui, server)

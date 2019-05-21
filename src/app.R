## app2.R ##
library(shiny)
library(shinydashboard)
library(plotly)
#---- UI ----
ui <- dashboardPage(
  skin = "black",
  header = dashboardHeader(title = "hcapca", titleWidth = 100),
  #---- Sidebar content ----
  sidebar = dashboardSidebar(width = 100, sidebarMenu(
    menuItem(
      text = "Tree",
      tabName = "tree",
      icon = icon("tree")
    ),
    menuItem(
      text = "PCA",
      tabName = "pca",
      icon = icon("desktop")
    ),
    menuItem(
      text = "Exit",
      icon = icon('power-off'),
      tabName = "powerOff"
    )
  )),
  #---- Body Content ----
  body = dashboardBody(tabItems(
    #---- First Tab ----
    tabItem(tabName = "tree",
            fluidRow(
              box(
                title = "Overall Tree",
                solidHeader = T,
                status = "info",
                dendroNetworkOutput(outputId = 'dendroNetwork'),
                width = 12
              )
            ),
            fluidRow(
              column(
                width = 2,
                box(
                  title = "Node Selection",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = NULL,
                  selectInput(
                    inputId = "node_to_plot_tree",
                    label = "Select Node to View Dendrogram",
                    choices = c("None", nodeNames),
                    selected = "None"
                  )
                )

              ),
              column(
                width = 10,
                box(
                  id = 'nodeBox',
                  title = paste0("Dendrogram"),
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = NULL,
                  dendroNetworkOutput(outputId = 'node')
                )
              )
            )),
    #---- Second tab ----
    tabItem(tabName = 'pca',
            fluidRow(
              column(
                width = 2,
                #---- Column 1 ----
                box(
                  title = "Node Selection",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  selectInput(
                    inputId = "node_to_plot_pca",
                    label = "Select Node to View PCA",
                    choices = c("None", nodeNames),
                    selected = "None"
                  )
                ),
                uiOutput(outputId = 'x_axis_placeholder'),
                uiOutput(outputId = 'y_axis_placeholder'),
                uiOutput(outputId = 'N_points_placeholder'),
                uiOutput(outputId = 'make_plots_placeholder')#,
                # verbatimTextOutput(outputId = "debug")
              ),
              column(
                width = 10,
                #---- Column 2 ----
                box(
                  title = "Scores",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = NULL,
                  plotlyOutput("scores")
                ),
                box(
                  title = "Loadings",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = NULL,
                  plotlyOutput("loadings")
                )
              )
            )),
    tabItem(
      tabName = 'powerOff',
      actionButton(inputId = 'power_off', label = '', icon = icon('power-off'))
    )
  ))
)

#---- Server functions ----
server <- function(input, output, session) {
  #---- 1. Tree Stuff ----
  # overall tree
  output$dendroNetwork <- renderDendroNetwork({
    dendroNetwork(
      hc = hc,
      fontSize = 20,
      # nodeColour = colors[3],
      nodeStroke = colors[3],
      linkColour = colors[4],
      opacity = 100
    )
  })

  dendro <- reactive({
    if (input$node_to_plot_tree != "None") {
      dend <-
        master_list[[get_node_position(input$node_to_plot_tree)]]$dend
      dend <- as.hclust(dend)
      return(dend)
    }
    else
      return()
  })

  output$node <- renderDendroNetwork({
    dendroNetwork(
      hc = dendro(),
      fontSize = 20,
      # nodeColour = colors[1],
      nodeStroke = colors[1],
      linkColour = colors[4]
    )
  })

  #---- 2. PCA stuff ----
  length_node_to_plot_pca <- reactive({
    l <-
      master_list[[get_node_position(input$node_to_plot_pca)]]$members
    l <- length(l)
    return(l)
  })
  observeEvent(eventExpr = {
    # if (input$more == 1) TRUE
    if (input$node_to_plot_pca != "None")
      TRUE
    else
      return()
  },
  handlerExpr = {
    output$x_axis_placeholder <-
      renderUI(
        box(
          id = 'x_axis_box',
          title = "X Axis",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          selectInput(
            inputId = "x_axis",
            label = "Select principal component for X",
            choices = 1:length_node_to_plot_pca(),
            selected = 1
          )
        )
      )
    output$y_axis_placeholder <-
      renderUI(
        box(
          id = 'y_axis_box',
          title = "Y Axis",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          selectInput(
            inputId = "y_axis",
            label = "Select principal component for Y",
            choices = 1:length_node_to_plot_pca(),
            selected = 2
          )
        )
      )
    output$N_points_placeholder <-
      renderUI(
        box(
          title = "Points on Loadings",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          numericInput(
            inputId = "N_points",
            label = "Points on Loadings Plot",
            value = 1000,
            min = 50,
            max = 10000,
            step = 50
          )
        )
      )
    output$make_plots_placeholder <-
      renderUI(
        actionButton(inputId = 'make_plots', label = 'Plot')
      )
  })
  # output$debug <- renderPrint({
  #   # print("input$node_to_plot_tree")
  #   # print(input$node_to_plot_tree)
  #   print("input$node_to_plot_pca")
  #   print(input$node_to_plot_pca)
  #   print("input$x_axis")
  #   print(input$x_axis)
  #   print("input$y_axis")
  #   print(input$y_axis)
  #   print("N_points")
  #   print(input$N_points)
  # })
  #---- Calculations for Scores and Loadings ----
  scoresData <- eventReactive(eventExpr = input$make_plots,
                              valueExpr = {
                                # Get pca
                                n <-
                                  get_node_position(input$node_to_plot_pca)
                                df <-
                                  remove_zeros(df[master_list[[n]]$members, ])
                                df.s <-
                                  scale_pareto(df) # pareto scaled data
                                pca <-
                                  prcomp(df.s, scale. = F, center = F)

                                xaxis <- as.numeric(input$x_axis)
                                yaxis <- as.numeric(input$y_axis)

                                #---- Loadings ----
                                df_loadings <-
                                  data.frame(pca$rotation[, c(xaxis, yaxis)])
                                df_loadings <-
                                  add_euclidean_distance(df_loadings)
                                df_loadings <-
                                  select_top_N_points(df = df_loadings, N_points = input$N_points)
                                df_loadings <-
                                  remove_euclidean_distance_column(df_loadings)

                                plt_loadings <- plot_ly(
                                  data = df_loadings,
                                  x = as.formula(paste0("~PC", xaxis)),
                                  y = as.formula(paste0("~PC", yaxis)),
                                  type = "scatter",
                                  mode = "markers",
                                  marker = list(
                                    size = 10,
                                    color = 'rgba(247, 15, 15, 0.9)',
                                    line = list(color = 'rgba(120, 7, 7, 0.8',
                                                width = 2)
                                  ),
                                  hoverinfo = 'text',
                                  text = ~ rownames(df_loadings)
                                ) %>% layout(title = paste0('Loadings Plot: PC', input$x_axis, ' v PC', input$y_axis))
                                #---- Scores ----
                                df_scores <-
                                  data.frame(pca$x[, c(xaxis, yaxis)])
                                plt_scores <-
                                  plot_ly(
                                    data = df_scores,
                                    x = as.formula(paste0("~PC", xaxis)),
                                    y = as.formula(paste0("~PC", yaxis)),
                                    type = "scatter",
                                    mode = "markers",
                                    marker = list(
                                      size = 10,
                                      color = 'rgba(15, 127, 191, 0.9)',
                                      line = list(color = 'rgba(5, 42, 64, 0.8',
                                                  width = 2)
                                    ),
                                    hoverinfo = 'text',
                                    text = ~ rownames(df_scores)
                                  ) %>% layout(title = paste0('Loadings Plot: PC', input$x_axis, ' v PC', input$y_axis))

                                return(
                                  list(
                                    'pca' = pca,
                                    'df_scores' = df_scores,
                                    'df_loadings' = df_loadings,
                                    'colnames' = c(colnames(df_loadings)),
                                    'plt_scores' = plt_scores,
                                    'plt_loadings' = plt_loadings
                                  )
                                )
                              })
  #---- output scores ----
  output$scores <- renderPlotly({
    scoresData()$plt_scores
  })
  #---- output loadings ----
  output$loadings <- renderPlotly({
    scoresData()$plt_loadings
  })
  observeEvent(eventExpr = input$power_off, handlerExpr = {
    stopApp(1)
  })
}

shinyApp(ui, server)

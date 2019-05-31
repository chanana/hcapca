#!/usr/bin/env Rscript
suppressMessages(library(shiny)) # shiny
suppressMessages(library(config)) # config file
suppressMessages(library(data.tree))
suppressMessages(library(collapsibleTree)) # collapsible Trees
suppressMessages(library(shinydashboard))
suppressMessages(library(dendextend))
suppressMessages(library(stringr))
suppressMessages(library(plotly))

# load configuration and functions
parameters <- config::get(file = "config_file.yaml")
source(
  file.path(
    parameters$input_folder_accessory_script,
    "accessory_functions.R"
  ),
  local = TRUE
)

# load master_list (list of nodes with dendrograms)
master_list <-
  readRDS(file = file.path(parameters$save_folder, "master_list_obj.RDS"))
nodeNames <- unlist(lapply(master_list, `[[`, "ID"))
colors <-
  readRDS(file = file.path(parameters$save_folder, "colors_obj.RDS"))
pca_objects <-
  list.files(
    path = file.path(parameters$save_folder),
    pattern = "pca",
    full.names = T
  )
pca_objects_str <-
  str_extract(string = pca_objects, pattern = "b[01]*")

#---- UI ----
ui <- dashboardPage(
  skin = "black",
  header = dashboardHeader(title = "hcapca", titleWidth = 100),
  #---- Sidebar content ----
  sidebar = dashboardSidebar(
    width = 100,
    sidebarMenu(
      menuItem(
        text = "Info",
        tabName = "info",
        icon = icon("info")
      ),
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
    )
  ),
  #---- Body Content ----
  body = dashboardBody(tabItems(
    #---- info Tab ----
    tabItem(tabName = 'info',
            fluidRow(
              box(
                title = "",
                solidHeader = F,
                status = "success",
                width = 8,
                includeHTML("text/Instructions2.html")
              )
            )),
    #---- tree Tab ----
    tabItem(tabName = "tree",
            fluidRow(
              box(
                title = "What is HCA?",
                solidHeader = F,
                status = 'success',
                width = 8,
                collapsible = T,
                includeHTML("text/1.1 Overall Tree.html")
              ),
              box(
                title = "Overall Tree",
                solidHeader = T,
                status = "info",
                collapsible = T,
                collapsibleTreeOutput(outputId = 'collapsibleTree'),
                width = 12
              )
            ),
            fluidRow(
              column(
                width = 2,
                box(
                  title = "Dendrogram",
                  solidHeader = F,
                  status = 'success',
                  width = NULL,
                  collapsible = T,
                  includeHTML("text/1.2 Dendrogram.html")
                ),
                box(
                  title = "Node Selection",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = T,
                  width = NULL,
                  selectInput(
                    inputId = "node_to_plot_tree",
                    label = "Select Node to View Dendrogram",
                    choices = nodeNames,
                    selected = "b"
                  )
                ),
                box(
                  title = "Node Selected",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  verbatimTextOutput('node_selected')
                )
              ),
              column(
                width = 10,
                box(
                  id = 'nodeBox',
                  title = "Dendrogram",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = NULL,
                  plotOutput(outputId = 'node')
                )
              )
            )),
    #---- pca tab ----
    tabItem(tabName = 'pca',
            fluidRow(
              box(
                title = "Selecting Parameters",
                solidHeader = F,
                status = 'success',
                width = 12,
                collapsible = T,
                includeHTML("text/2.1 Parameters.html")
              )
            ),
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
                uiOutput(outputId = 'make_plots_placeholder'),
                infoBoxOutput(outputId = 'pc1_pc2', width = NULL)
                # verbatimTextOutput(outputId = "debug")
              ),
              column(
                width = 10,
                #---- Column 2 ----
                box(
                  title = "Scores Plot",
                  solidHeader = F,
                  status = 'success',
                  width = NULL,
                  collapsible = T,
                  includeHTML("text/2.2 Scores Plot.html")
                ),
                box(
                  title = "Scores",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = T,
                  width = NULL,
                  plotlyOutput("scores")
                ),
                box(
                  title = "Loadings Plot",
                  solidHeader = F,
                  status = 'success',
                  width = NULL,
                  collapsible = T,
                  includeHTML("text/2.3 Loadings Plot.html")
                ),
                box(
                  title = "Loadings",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  plotlyOutput("loadings")
                ),
                box(
                  title = "Variance",
                  solidHeader = F,
                  status = 'success',
                  width = NULL,
                  collapsible = T,
                  includeHTML("text/2.4 Cumulative and Individual Variance.html")
                ),
                box(
                  title = "Individual and Cumulative Variance",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  plotlyOutput("variance")
                )
              )
            )),
    # poweroff tab
    tabItem(tabName = 'powerOff',
            fluidRow(
              column(width = 4,
              box(
                title = "Exit",
                solidHeader = F,
                status = 'success',
                width = NULL,
                collapsible = T,
                includeHTML("text/3. Exit.html")
              ),
              actionButton(width = NULL,
                inputId = 'power_off',
                label = '',
                icon = icon('power-off')
              ))
            ))
  ))
)

#---- Server functions ----
server <- function(input, output, session) {
  #---- 1. Tree Stuff ----

  # overall tree - first box; tree tab
  output$collapsibleTree <- renderCollapsibleTree({
    collapsibleTree(
      df = get_tree(master_list),
      fontSize = 20,
      collapsed = F,
      inputId = 'cTnode'
    )
  })

  # reactive for plotting dendrogram based on selected node; tree tab
  dendro <- reactive({
    dend <-
      master_list[[get_node_position(input$node_to_plot_tree)]]$dend
    return(dend)
  })

  # plot of dendrogram; tree tab
  output$node <- renderPlot({
    par(bg = colors[6], fg = colors[4])
    d <- dendro()
    d %>%
      # dendextend::set("branches_lwd", 2) %>%
      color_branches(k = 2, col = colors[c(1, 3)]) %>%
      hang.dendrogram %>%
      plot(panel.first = {
        grid(
          col = colors[8],
          lty = 3,
          nx = NA,
          ny = NULL
        )
      })
  })
  output$node_selected <- renderPrint({
    list_of_nodes <-
      unlist(input$cTnode) # comes from collapsibleTree inside renderCT above
    cat("Node you clicked: ", tail(list_of_nodes, 1), "\n")
    cat("Parent nodes: ", head(list_of_nodes, -1))
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
            value = parameters$max_points_loadings,
            min = 50,
            max = 10000,
            step = 50
          )
        )
      )
    output$make_plots_placeholder <-
      renderUI(
        box(
          title = "",
          status = "success",
          solidHeader = FALSE,
          collapsible = FALSE,
          width = NULL,
          actionButton(inputId = 'make_plots', label = 'Plot')
        )
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
  pcaData <- eventReactive(eventExpr = input$make_plots,
                           valueExpr = {
                             # Get pca object from file
                             position <-
                               match(x = input$node_to_plot_pca,
                                     table = pca_objects_str)
                             pca <-
                               readRDS(file = pca_objects[position])

                             xaxis <- as.numeric(input$x_axis)
                             yaxis <- as.numeric(input$y_axis)

                             #---- Loadings ----
                             df_loadings <-
                               data.frame(pca$rotation[, c(xaxis, yaxis)])
                             df_loadings <-
                               add_euclidean_distance(df_loadings)
                             df_loadings <-
                               select_top_N_points(df = df_loadings,
                                                   N_points = input$N_points)
                             df_loadings <-
                               remove_euclidean_distance_column(df_loadings)

                             hovertext <- paste0(
                               "</br>M/Z: ",
                               lapply(strsplit(rownames(df_loadings), "_"), `[[`, 1),
                               "</br>RT: ",
                               lapply(strsplit(rownames(df_loadings), "_"), `[[`, 2))

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
                               text = ~ hovertext
                             ) %>% layout(title = paste0('Loadings Plot: PC', input$x_axis,
                                                         ' v PC', input$y_axis))

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
                               ) %>% layout(title = paste0('Scores Plot: PC', input$x_axis,
                                                           ' v PC', input$y_axis))

                             #---- Variance ----
                             proportion_of_variance <-
                               summary(pca)$importance[2, ] * 100
                             cumulative_proportion <-
                               summary(pca)$importance[3, ] * 100
                             xaxis <-
                               1:length(proportion_of_variance)

                             plt_variance <-
                               plot_ly(
                                 data = data.frame(cumulative_proportion),
                                 type = 'scatter',
                                 mode = 'lines+markers',
                                 x = xaxis,
                                 y = cumulative_proportion,
                                 hoverinfo = 'text',
                                 text = ~ paste0(
                                   "</br> PC",
                                   xaxis,
                                   "</br>Explains: ",
                                   proportion_of_variance,
                                   "%",
                                   "</br>Cumulative: ",
                                   cumulative_proportion,
                                   "%"
                                 )
                               ) %>% layout(title = "Cumulative Variance")

                             return(
                               list(
                                 'pca' = pca,
                                 'df_scores' = df_scores,
                                 'df_loadings' = df_loadings,
                                 'pc1_pc2' = cumulative_proportion[2],
                                 'colnames' = c(colnames(df_loadings)),
                                 'plt_scores' = plt_scores,
                                 'plt_loadings' = plt_loadings,
                                 'plt_variance' = plt_variance
                               )
                             )
                           })
  #---- output scores ----
  output$scores <- renderPlotly({
    pcaData()$plt_scores
  })
  #---- output loadings ----
  output$loadings <- renderPlotly({
    pcaData()$plt_loadings
  })
  #---- output cumulative variance ----
  output$variance <- renderPlotly({
    pcaData()$plt_variance
  })
  #---- output pc1+pc2 ----
  output$pc1_pc2 <- renderInfoBox({
    infoBox(title = "PC1 + PC2 explain", subtitle = "Variance",
      value = paste0(round(pcaData()$pc1_pc2, 2), "%"),
      icon = icon('chart-bar'),
      color = 'purple',
    )
  })
  #---- Power Button ----
  observeEvent(eventExpr = input$power_off, handlerExpr = {
    stopApp(1)
  })
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)

#!/usr/bin/env Rscript
suppressMessages(library(shiny)) # shiny
suppressMessages(library(config)) # config file
suppressMessages(library(data.tree))
suppressMessages(library(collapsibleTree)) # collapsible Trees
suppressMessages(library(shinydashboard))
suppressMessages(library(dendextend))
suppressMessages(library(stringr))
suppressMessages(library(plotly))
suppressMessages(library(DT))


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
colors <-
  readRDS(file = file.path(parameters$save_folder, "colors_obj.RDS"))
palette(colors)
pca_objects <-
  list.files(
    path = file.path(parameters$save_folder),
    pattern = "pca",
    full.names = T
  )
pca_objects_str <-
  str_extract(string = pca_objects, pattern = "b[01]*")
nodeNames <- unlist(lapply(master_list, `[[`, "name"))
nodeNamesForSelecting <- nodeNames
nodeIDs <- unlist(lapply(master_list, `[[`, "ID"))
names(nodeIDs) <- nodeNames[nodeIDs]
names(nodeNamesForSelecting) <- NULL
complete_data_frame <- readRDS(file=file.path(parameters$save_folder, "df_obj.RDS"))

# load metadata if present
if (!is.null(parameters$metadata)) {
  metadata <-
    readRDS(file = file.path(parameters$save_folder, "metadata_obj.RDS"))
  color_column_names <-
    colnames(metadata)[str_detect(string = colnames(metadata), pattern = "color")]
}
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
        text = "Table",
        icon = icon("table"),
        tabName = "dataTables"
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
                collapsible = TRUE,
                collapsed = TRUE,
                width = 8,
                includeHTML("text/Instructions2.html")
              )
            )),
    #---- tree Tab ----
    tabItem(tabName = "tree",
            fluidRow(
              box(
                title = "What is HCA?",
                solidHeader = FALSE,
                status = 'success',
                width = 8,
                collapsible = TRUE,
                collapsed = TRUE,
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
                  solidHeader = FALSE,
                  status = 'success',
                  width = NULL,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  includeHTML("text/1.2 Dendrogram.html")
                ),
                box(
                  title = "Node Selection",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  selectInput(
                    inputId = "node_to_plot_tree",
                    label = "Select Node to View Dendrogram",
                    choices = nodeNamesForSelecting,
                    selected = nodeNamesForSelecting[1]
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
    tabItem(
      tabName = 'pca',
      fluidRow(column(
        width = 12,
        box(
          title = "Selecting Parameters",
          solidHeader = FALSE,
          status = 'success',
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          includeHTML("text/2.1 Parameters.html")
        ),
        box(
          title = "Overall Tree",
          solidHeader = T,
          status = "info",
          collapsible = T,
          collapsibleTreeOutput(outputId = 'collapsibleTreePCA'),
          width = NULL
        ),
        box(
          title = "Node Selected",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          verbatimTextOutput('node_selected_pca')
        )
      )),
      #---- Selection Boxes ----
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
              choices = c("None", nodeNamesForSelecting),
              selected = "None"
            )
          )
        ),
        column(width = 2,
               uiOutput(outputId = 'x_axis_placeholder')),
        column(width = 2,
               uiOutput(outputId = 'y_axis_placeholder')),
        column(width = 2,
               uiOutput(outputId = 'N_points_placeholder')),
        column(width = 2,
               uiOutput(outputId = 'make_plots_placeholder')),
        column(width = 2,
               infoBoxOutput(outputId = 'pc1_pc2', width = NULL))
      ),
      fluidRow(column(width = 12,
                      box(
                        title = "Debug Info",
                        status = "info",
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        width = NULL,
                        verbatimTextOutput('debugInfo')
                      ))),
      #---- Scores/Loadings plots ----
      fluidRow(column(
        width = 6,
        box(
          title = "Scores Plot",
          solidHeader = FALSE,
          status = 'success',
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          includeHTML("text/2.2 Scores Plot.html")
        ),
        box(
          title = "Scores",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          plotlyOutput("scores")
        )
      ),
      column(
        width = 6,
        box(
          title = "Loadings Plot",
          solidHeader = FALSE,
          status = 'success',
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          includeHTML("text/2.3 Loadings Plot.html")
        ),
        box(
          title = "Loadings",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          plotlyOutput("loadings")
        )
      )),
      fluidRow(column(
        width = 12,
        box(
          title = "Variance",
          solidHeader = FALSE,
          status = 'success',
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          includeHTML("text/2.4 Cumulative and Individual Variance.html")
        ),
        box(
          title = "Individual and Cumulative Variance",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          plotlyOutput("variance")
        )
      )),
      #---- Table on PCA Node ----
      fluidRow(
        column(
          width = 12,
          box(
            title = "Table",
            solidHeader = TRUE,
            status = 'danger',
            width = NULL,
            collapsible = TRUE,
            div(
              style = 'overflow-x: scroll',
              DT::dataTableOutput(outputId = 'table_of_node_pca_tab')
            )
          )
        ))
    ),
    #---- poweroff tab ----
    tabItem(tabName = 'powerOff',
            fluidRow(column(
              width = 4,
              box(
                title = "Exit",
                solidHeader = F,
                status = 'success',
                width = NULL,
                collapsible = T,
                includeHTML("text/3. Exit.html")
              ),
              actionButton(
                width = NULL,
                inputId = 'power_off',
                label = '',
                icon = icon('power-off')
              )
            )))
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
  output$collapsibleTreePCA <- renderCollapsibleTree({
    collapsibleTree(
      df = get_tree(master_list),
      fontSize = 20,
      collapsed = F,
      inputId = 'cTnode_pca'
    )
  })
  # reactive for plotting dendrogram based on selected node; tree tab
  dendro <- reactive({
    dend <-
      master_list[[get_node_position_by_name(input$node_to_plot_tree)]]$dend
    return(dend)
  })

  # plot of dendrogram; tree tab
  output$node <- renderPlot({
    d <- dendro()
    # if metadata file exists, then make colored dendrograms
    if (exists(x = "metadata", inherits = TRUE)) {
      color_vector_list <- list()
      for (col in color_column_names) {
        df_for_legend <- metadata[match(labels(d), metadata[, 1]),]
        color_vector_list[[col]] <- df_for_legend[, col]
      }
      if (length(color_vector_list) == 2) {
        # this means there is a second category with colors so we should
        # color the labels to show this category
        labels_colors(d) <- color_vector_list[[2]]
      }
      # number_of_members <- length(labels(d)) # get total number of leaves
      # w = width_of_pdf(numberOfLeaves = number_of_members) # get width
      d %>%
        dendextend::set("branches_lwd", 2) %>% # make branches' lines wider
        dendextend::set("leaves_pch", 16) %>% # make leaves points circles
        dendextend::set("leaves_cex", 2.5) %>% # make circles bigger
        dendextend::set("leaves_col", color_vector_list[[color_column_names[1]]]) %>% # assign colors to circles
        # hang.dendrogram %>%
        plot(panel.first = {
          grid(
            col = '#5e5e5e',
            lty = 3,
            nx = NA,
            ny = NULL
          )
        })
      if (length(color_vector_list) == 1) {
        legend(
          "topright",
          legend = unique(df_for_legend[, 2]),
          fill = unique(df_for_legend[, 3]),
          border = "black",
          cex = 1,
          y.intersp = 0.7,
          ncol = 2,
          title = paste0(colnames(metadata)[2], " (circles)")
        )
      }
      if (length(color_vector_list) == 2) {
        legend(
          "topright",
          legend = unique(df_for_legend[, 2]),
          fill = unique(df_for_legend[, 4]),
          border = "black",
          cex = 1,
          y.intersp = 0.7,
          ncol = 2,
          title = paste0(colnames(metadata)[2], " (circles)")
        )
        legend(
          'topleft',
          legend = unique(df_for_legend[, 3]),
          fill = unique(df_for_legend[, 5]),
          border = "black",
          cex = 1,
          y.intersp = 0.7,
          ncol = 2,
          title = paste0(colnames(metadata)[3], " (labels)")
        )
      }
    } else {
      # else just make normal ones
      par(bg = "#d3d3d3", fg = '#000000')
      d %>%
        # dendextend::set("branches_lwd", 2) %>%
        dendextend::color_branches(k = 2, col = 1:2) %>%
        hang.dendrogram %>%
        plot(panel.first = {
          grid(
            col = '#ffffff',
            lty = 3,
            nx = NA,
            ny = NULL
          )
        })
    }
  })
  output$node_selected <- renderPrint({
    list_of_nodes <-
      unlist(input$cTnode) # comes from collapsibleTree inside renderCT above
    cat("Node you clicked: ", tail(list_of_nodes, 1), "\n")
    cat("Parent nodes: ", head(list_of_nodes, -1))
  })
  output$node_selected_pca <- renderPrint({
    list_of_nodes <-
      unlist(input$cTnode_pca) # comes from collapsibleTree inside renderCT above
    cat("Node you clicked: ", tail(list_of_nodes, 1), "\n")
    cat("Parent nodes: ", head(list_of_nodes, -1))
  })

  #---- 2. PCA stuff ----
  length_node_to_plot_pca <- reactive({
    l <-
      master_list[[get_node_position_by_name(input$node_to_plot_pca)]]$members
    l <- length(l)
    return(l)
  })
  #---- Selection boxes appear ----
  observeEvent(eventExpr = {
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
  pcaData <-
    eventReactive(
      eventExpr = input$make_plots,
      valueExpr = {
        # Get pca object from file
        position <-
          match(x = nodeIDs[input$node_to_plot_pca],
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
        hovertext <-
          paste0(#rownames(df_loadings))
            "</br>M/Z: ",
            lapply(strsplit(rownames(df_loadings), "_"), `[[`, 1),
            "</br>RT: ",
            lapply(strsplit(rownames(df_loadings), "_"), `[[`, 2))

        plt_loadings <- plot_ly(
          source = "L",
          data = df_loadings[, 1:2],
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
        ) %>% layout(title = paste0('Loadings Plot: PC', xaxis,
                                    ' v PC', yaxis),
                     dragmode = "select")

        #---- Scores ----
        df_scores <-
          data.frame(pca$x[, c(xaxis, yaxis)])
        plt_scores <-
          plot_ly(
            source = "S",
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
          ) %>% layout(title = paste0('Scores Plot: PC', xaxis,
                                      ' v PC', yaxis))

        #---- Variance ----
        proportion_of_variance <-
          summary(pca)$importance[2,] * 100
        cumulative_proportion <-
          summary(pca)$importance[3,] * 100
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
          )

        return(
          list(
            # 'pca' = pca,
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
  #---- Loadings plot table ----
  output$table_of_node_pca_tab <- DT::renderDataTable(expr = {
    req(pcaData())
    eventData_drag_L <-
      event_data(event = "plotly_selected", source = "L")

    if (!is.null(eventData_drag_L)) {
      # this means it's been zoomed
      rownames_for_table <- rownames(# the +1 is because pointNumbers are numbered from 0
        pcaData()$df_loadings[eventData_drag_L$pointNumber + 1,])
      column_names_for_table <-
        rownames(pcaData()$df_scores)
      dff <-
        complete_data_frame[column_names_for_table, rownames_for_table]
      dff <- data.frame(t(as.matrix(dff)))
      colnames(dff) <- column_names_for_table
      return(dff)
    }
  })
  #---- Debug ----
  output$debugInfo <- renderPrint({
    req(pcaData())
    eventData_hover_S <- event_data(event = "plotly_hover", source = "S")
    print("plotly_hover")
    print(eventData_hover_S)
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
    infoBox(
      title = "PC1 + PC2",
      subtitle = "Variance",
      value = paste0(round(pcaData()$pc1_pc2, 2), "%"),
      # icon = icon('chart-bar'),
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

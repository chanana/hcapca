# ~~~~ accessory functions ~~~~
# returns N two letter node names (TLNN) for human readability of node Names
two_letter_node_Names <- function(N = 0) {
  a = rep(NA, length(letters) ^ 2)
  count = 1
  for (nodeID in letters) {
    for (j in letters) {
      a[count] = paste0(nodeID, j)
      count = count + 1
    }
  }
  TLNN <- sample(x = a,
                 size = N,
                 replace = FALSE)
  return(TLNN)
}

change_pathString <- function(oldPathString, dict = TLNN) {
  oldPathString <-
    unlist(str_split(string = oldPathString, pattern = "/"))
  newPathString <-
    paste(dict[oldPathString], collapse = "/")
  return(newPathString)
}

# returns a list of objects who do not inherit from class "function"
ClassFilter <- function(x) {
  !inherits(base::get(x), 'function')
}

# Saves object to <object-name>_obj file in save_points directory
save_object <- function(saveDirectory,
                        object = NULL,
                        objectName = NULL,
                        objectEnv = .GlobalEnv) {
  # object must be given as a string (in quotes)
  # objects must be reloaded with readRDS()
  if (!dir.exists(saveDirectory)) {
    dir.create(saveDirectory)
  }
  if (is.null(object)) {
    # save all objects except functions
    objNames <- Filter(ClassFilter, ls(envir = .GlobalEnv))
    for (obj in objNames) {
      objName <- file.path(saveDirectory, paste0(obj, "_obj.RDS"))
      saveRDS(
        object = base::get(obj),
        file = objName,
        compress = T

      )
    }
  }
  else {
    if (is.null(objectName)) {
      objName = file.path(saveDirectory, paste0(object, "_obj.RDS"))
    }
    else {
      objName = file.path(saveDirectory, paste0(objectName, "_obj.RDS"))
    }
    saveRDS(
      object = base::get(x = object, envir = objectEnv),
      file = objName,
      compress = T
    )
  }
}

scale_pareto <- function(df) {
  #returns pareto scaled data
  df = as.data.frame(apply(
    X = df,
    MARGIN = 2,
    FUN = function(x)
      (x - mean(x)) / sqrt(sd(x))
  ))
}

remove_nas <- function(df) {
  df <- df[, unlist(lapply(df, function(x)
    ! all(is.na(x))))]
  return(df)
}

# remove_zero_columns <- function(df) {
#   df <- df[, which(colSums(df) != 0)]
# }

euclidean_distance <- function(x) {
  sqrt(x[1] ^ 2 + x[2] ^ 2)
}

add_euclidean_distance <- function(df) {
  df <- as.data.frame(df)
  df$euc <- apply(X = df, MARGIN = 1, euclidean_distance)
  return(df)
}

select_top_N_points <- function(df, N_points = 5000) {
  # warning: loadings plot must have 3 columns; first two are coordinates,
  # last one is euclidean distance named euc
  df <- df[order(df$euc, decreasing = TRUE),]
  df <- df[1:N_points,]
  return(df)
}

remove_euclidean_distance_column <- function(df) {
  return(df[, -3])
}

get_highest_PC <- function(mtx) {
  # takes a scores plot and returns a dataframe containing each sample sorted by
  # the top two PCs where that sample stands out.
  df.sorted <- as.data.frame(apply(
    X = abs(mtx),
    MARGIN = 1,
    FUN = function(x)
      sort(x, decreasing = T, index.return = T)
  ))
  df.sorted <-
    df.sorted[1:2, grep(pattern = ".ix", x = colnames(df.sorted))]
  colnames(df.sorted) <- rownames(mtx)
  rownames(df.sorted) <- c("PC.x", "PC.y")
  return(df.sorted)
}

convert_tree_to_dataframe <- function(LIST) {
  x <-
    data.frame(
      'pathString' = unlist(lapply(
        X = LIST,
        FUN = function(x)
          x$pathString
      )),
      'number' = unlist(lapply(
        X = LIST,
        FUN = function(x)
          length(x$members)
      )),
      'isLeaf' = unlist(lapply(
        X = LIST,
        FUN = function(x)
          ifelse(x$isLeaf, "Yes", "No")
      )),
      'PC1_PC2' = unlist(lapply(
        X = LIST,
        FUN = function(x)
          round(x$var[1] + x$var[2], 2)
      )),
      'links' = unlist(lapply(
        X = LIST,
        FUN = function(x)
          x$links
      ))
    )
  return(x)
}

get_tree <- function(LIST) {
  x <- as.Node(convert_tree_to_dataframe(LIST))
  return(x)
}

display_tree <- function(LIST) {
  print(get_tree(LIST),
        'number',
        'isLeaf',
        'PC1_PC2',
        'links',
        limit = NULL)
}

get_node_position <- function(nodeID) {
  n <-
    which(lapply(
      X = master_list,
      FUN = function(x)
        x$ID == nodeID
    ) == 1)
  if (length(n) == 0) {
    return(-1)
  }
  else
    return(n)
}

get_node_position_by_name <- function(nodeName) {
  n <-
    which(lapply(
      X = master_list,
      FUN = function(x)
        x$name == nodeName
    ) == 1)
  if (length(n) == 0) {
    return(-1)
  }
  else
    return(n)
}

unexplored_nodes <- function(nameOrNumber) {
  # returns a vector of node names or numbers
  if (identical(nameOrNumber, "name")) {
    v = unlist(lapply(
      X = master_list,
      FUN = function(x) {
        if (x$isLeaf) {
          x$ID
        }
      }
    ))
    return(v)
  }
  if (identical(nameOrNumber, "number")) {
    v = unlist(lapply(
      X = master_list,
      FUN = function(x) {
        if (x$isLeaf) {
          length(x$members)
        }
      }
    ))
    return(v)
  }
}

pca_based_on_membership <-
  function(dataframe = df,
           name_list = NULL) {
    # scale -> make correlation matrix -> cluster ->
    # make dendrogram -> add labels
    df.s <- scale_pareto(df[name_list,]) # pareto scaled data
    df.s <- remove_nas(df.s)
    df.pca <- prcomp(df.s, scale. = F, center = F)
    return(df.pca)
  }

pca_based_on_node <- function(dataframe = df, nodeName) {
  n <- get_node_position(nodeName)
  p <-
    pca_based_on_membership(dataframe = dataframe, name_list = master_list[[n]]$members)
  return(p)
}

explained_variance_of_node <-
  function(dataframe = df,
           nodeID = NULL,
           position = NULL,
           saveDirectory = saveDirectory) {
    # returns a vector of explained variance for a given node; hopefully not all
    # three are null at any one time

    # retrieve node from list if name supplied
    if (is.null(position)) {
      n <- get_node_position(nodeID)
    } else {
      n <- position
    }

    # if variance array has already been calculated, return it
    nodeVariance = master_list[[n]]$var
    if (!is.null(nodeVariance)) {
      return(nodeVariance)
    } else {
      pca.env <<- new.env()
      # calculate pca of node based on its members if is supplied instead
      pca.env$p <-
        pca_based_on_membership(dataframe = dataframe,
                                name_list = master_list[[n]]$members)

      save_object(
        saveDirectory = saveDirectory,
        object = "p",
        objectName = paste0(master_list[[n]]$ID, "_pca"),
        objectEnv = pca.env
      )

      # the explained variance in a vector
      v <- pca.env$p$sdev ^ 2 / sum(pca.env$p$sdev ^ 2) * 100
      return(v)
    }
  }

process_node <- function(dataframe = df, id) {
  # finds the element in the list with the ID of id
  n <- get_node_position(id)

  cat(paste0("\n", "[1/4] found node at position: ", n))

  # node is then just an element from the master_list with the correct id
  node <- master_list[[n]]

  # Get dataframe
  df <- dataframe[node$members,]

  # Scale, get distance metric, cluster, and get dendrogram with labels
  df.s <- scale_pareto(df) # pareto scale the data
  df.s <- remove_nas(df.s) # remove any NaNs
  df.cor <- cor(x = t(df.s), y = t(df.s)) # correlation matrix
  df.clust <-
    hclust(as.dist(1 - df.cor), method = "average") # clusters
  df.dend <- as.dendrogram(df.clust) # as dendrogram object
  labels(df.dend) <-
    rownames(df)[order.dendrogram(df.dend)] # labels
  cat(paste0("\n", "[2/4] selected and processed data and made dendrogram"))

  # partition into two clusters
  membership <- cutree_1k.dendrogram(dend = df.dend, k = 2)

  # save the dendrogram in the node
  master_list[[n]]$dend <<- df.dend

  # find where to put the new nodes
  new_node_position <- length(master_list) + 1

  # left child [append a 0]
  leftChildID = paste0(node$ID, 0)
  leftChildMembers = names(which(membership == 1))
  leftChildPathString = paste(node$pathString, leftChildID, sep = "/")
  master_list[[new_node_position]] <<-
    list(
      ID = leftChildID,
      members = leftChildMembers,
      parent = node$ID,
      pathString = leftChildPathString,
      isLeaf = TRUE
    )
  cat(paste0("\n", "[3/4] made left child called: ", leftChildID))

  # right child [append a 1]
  rightChildID = paste0(node$ID, 1)
  rightChildMembers = names(which(membership == 2))
  rightChildPathString = paste(node$pathString, rightChildID, sep = "/")
  master_list[[new_node_position + 1]] <<-
    list(
      ID = rightChildID,
      members = rightChildMembers,
      parent = node$ID,
      pathString = rightChildPathString,
      isLeaf = TRUE
    )
  cat(paste0("\n", "[4/4] made right child called: ", rightChildID, "\n"))

  # set isLeaf option of current node to FALSE
  master_list[[n]]$isLeaf <<- FALSE
}

auto_process <- function(dataframe = df,
                         numOrVar = "num",
                         N = 30,
                         saveDirectory = saveDirectory) {
  # check if 'N' is a number
  if (!is.numeric(N)) {
    return("Not a number!")
  }

  # default assumes "num" but other option is "var" for a variance cutoff
  if (identical(numOrVar, "num")) {
    if (N %% 1 != 0) {
      return("Not a whole number!")
    } # must be a whole number for option 'num

    # if the tests pass, then we know to process based on a number cutoff
    while (TRUE) {
      a <- unexplored_nodes('name')
      b <- unexplored_nodes('number')

      # hopefully, the two vectors are the same length
      a <- a[b > N]

      # if no node meets the above criterion, a is a 0 length character vector
      if (identical(a, character(0))) {
        return("Finished auto-generating tree based on number.")
      }

      # process all nodes present in a
      for (nodeID in a) {
        cat("\n", "=====")
        process_node(dataframe = dataframe, id = nodeID)
        cat("=====", "\n", "Processed!", "\n")
      }
    }

  } else if (identical(numOrVar, "var")) {
    N <- round(N, 2) # round the input value to two decimals

    while (TRUE) {
      a <- unexplored_nodes('name') # get list of unexpanded nodes
      b <- c()
      e <-
        unexplored_nodes("number") # get list of how many members each has

      # if a node has only three or fewer members, it should not be expanded
      a <- a[e > 3]

      for (nodeID in a) {
        v <-
          explained_variance_of_node(
            dataframe = dataframe,
            nodeID = nodeID,
            saveDirectory = saveDirectory
          )
        n <- get_node_position(nodeID)
        master_list[[n]]$var <<- v
        b <- c(b, round(v[1] + v[2], 2))
      }

      # filter array based on the cutoff variance
      a <- a[b < N]

      if (identical(a, character(0))) {
        return("Finished auto-generating tree based on variance.")
      }

      for (nodeID in a) {
        cat('\n', '=====')
        process_node(dataframe = dataframe, id = nodeID)
        cat('\n', '=====', '\n', 'Processed!', '\n')
      }
    }

  } else {
    return("Please enter either 'num' or 'var'.")
  }
}

make_pca_plot <-
  function(nodeID,
           axis1 = 1,
           axis2 = 2,
           max_points_loadings = 5000) {
    p <- readRDS(file.path(
      getwd(),
      parameters$save_folder,
      paste0(nodeID, "_pca_obj.RDS")
    ))

    plt_scores <-
      plot_ly(
        data = as.data.frame(p$x[, c(axis1, axis2)]),
        x = as.formula(paste0("~PC", axis1)),
        y = as.formula(paste0("~PC", axis2)),
        type = "scatter",
        mode = "markers",
        # x = paste0("~PC",axis1), y = paste0("~PC", axis2),
        marker = list(
          size = 10,
          color = 'rgba(255, 182, 193, 0.9)',
          line = list(color = 'rgba(152, 0, 0, 0.8)',
                      width = 2)
        ),
        hoverinfo = 'text',
        text = ~ rownames(p$x)
      ) %>%
      layout(title = paste0('Scores Plot: PC', axis1, ' v PC', axis2))

    loadings <-
      add_euclidean_distance(df = p$rotation[, c(axis1, axis2)])
    loadings <-
      select_top_N_points(df = loadings, N_points = max_points_loadings)
    loadings <- remove_euclidean_distance_column(loadings)
    plt_loadings <-
      plot_ly(
        data = as.data.frame(loadings),
        x = as.formula(paste0("~PC", axis1)),
        y = as.formula(paste0("~PC", axis2)),
        type = "scatter",
        mode = "markers",
        # x = paste0("~PC",plane1), y = paste0("~PC",plane2),
        marker = list(
          size = 10,
          color = 'rgba(184, 184, 255, 0.9)',
          line = list(color = 'rgb(0, 0, 153, 0.8)',
                      width = 2)
        ),
        hoverinfo = 'text',
        text = ~ rownames(loadings)
      ) %>%
      layout(title = paste0('Loadings Plot: PC', axis1, ' v PC', axis2))

    return(list('scores' = plt_scores, 'loadings' = plt_loadings))
  }

make_pca_html <-
  function(nodeID,
           axis1 = 1,
           axis2 = 2,
           outfile = "pca",
           max_points_loadings = 5000) {
    pca <- make_pca_plot(
      nodeID = nodeID,
      axis1 = axis1,
      axis2 = axis2,
      max_points_loadings = max_points_loadings
    )
    filename_scores <-
      file.path(getwd(),
                outfile,
                paste0(nodeID, "_PC", axis1, "-", axis2, "_S.html"))
    htmlwidgets::saveWidget(widget = pca$scores,
                            file = filename_scores,
                            selfcontained = TRUE)

    filename_loadings <-
      file.path(getwd(),
                outfile,
                paste0(nodeID, "_PC", axis1, "-", axis2, "_L.html"))
    htmlwidgets::saveWidget(
      widget = pca$loadings,
      file = filename_loadings,
      selfcontained = TRUE
    )
  }

make_dendrogram <- function(dataframe = df, nodeName) {
  n <- get_node_position(nodeName)
  name_list <- master_list[[n]]$members

  df <- dataframe[name_list,]
  df.s <- scale_pareto(df)
  df.s <- remove_nas(df.s)
  df.cor <- cor(x = t(df.s), y = t(df.s)) #correlation matrix
  df.clust <-
    hclust(as.dist(1 - (df.cor)), method = "average") #clusters
  df.dend <- as.dendrogram(df.clust) #as dendrogram object
  labels(df.dend) <-
    rownames(df)[order.dendrogram(df.dend)] #label each leaf
  master_list[[n]]$dend <<- df.dend
  return(df.dend)
}

make_hca_plot_pdf <-
  function(dataframe = df,
           nodeID,
           width = 16,
           height = 9,
           lwd = 2,
           outfile = "hca") {
    n <- get_node_position(nodeID)
    number_of_members <- length(master_list[[n]]$members)
    d <- master_list[[n]]$dend

    width = width_of_pdf(numberOfLeaves = number_of_members) # get width

    if (is.null(d)) {
      d <- make_dendrogram(dataframe = dataframe, nodeName = nodeID)
    }

    filename <-
      file.path(getwd(),
                outfile,
                paste0(nodeID, "-", number_of_members, ".pdf"))

    if (!dir.exists(outfile)) {
      dir.create(outfile)
    } # create directory if it doesn't exist

    pdf(file = filename,
        width = width,
        height = height)
    par(bg = "#d3d3d3", fg = '#000000')
    d %>%
      dendextend::set("branches_lwd", lwd) %>%
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
    dev.off()
  }

make_colored_hca_plot_pdf <-
  function(dataframe = df,
           nodeID,
           outfile = "hca",
           metadata,
           color_column_names) {
    d <- master_list[[get_node_position(nodeID)]]$dend # get dendrogram

    if (is.null(d)) {
      d <- make_dendrogram(dataframe = dataframe, nodeID)
    }
    color_vector_list <- list()

    # filter metadata so that only matching names
    for (col in color_column_names) {
      df_for_legend <- metadata[match(labels(d), metadata[, 1]),]
      color_vector_list[[col]] <- df_for_legend[, col]
    }

    if (length(color_vector_list) == 2) {
      # this means there is a second category with colors so we should
      # color the labels to show this category
      labels_colors(d) <- color_vector_list[[2]]
    }

    number_of_members <-
      length(labels(d)) # get total number of leaves
    w = width_of_pdf(numberOfLeaves = number_of_members) # get width

    if (!dir.exists(outfile)) {
      dir.create(outfile)
    } # create directory if it doesn't exist

    filename <-
      file.path(getwd(),
                outfile,
                paste0(nodeID, "-", number_of_members, ".pdf"))
    pdf(file = filename,
        width = w,
        height = 9)
    # par(bg = "#E5E5E5", fg = "#5e5e5e")
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
    dev.off()

    # pdf(file = "legend.pdf", width=7, height=7)
    # plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    # legend("topright", legend = unique(metadata[,2]), fill = unique(metadata[,3]),
    #        border = "black", cex=0.7, y.intersp = 0.7, ncol=2)
    # dev.off()
  }

add_color_column <- function(metadata, column_names) {
  # make sure the columns are factors
  metadata[column_names] <- lapply(metadata[column_names], factor)

  # for each column, add a corresponding color column named <colname>.color
  for (name in column_names) {
    metadata[paste0(name, ".color")] <-
      as.numeric(unlist(metadata[name]))
  }
  return(metadata)
}

width_of_pdf <- function(numberOfLeaves) {
  # increase width by 16in for every 25 extra leaves
  w = (as.integer(numberOfLeaves / 50) + 1) * 16
  return(w)
}

find_strain <- function(strainName) {
  unlist(lapply(
    master_list,
    FUN = function(x) {
      if (length(which(x$members == strainName)) != 0) {
        x$ID
      }
    }
  ))
}

add_links_attribute <- function(nodeName) {
  node_pos <- get_node_position(nodeName)

  # HCA
  num_of_membs <- length(master_list[[node_pos]]$members)
  HCA <-
    paste0("<a href='hca/",
           nodeName,
           "-",
           num_of_membs,
           ".pdf'",
           ">HCA</a>")

  # PCA
  PCA_Load <-
    paste0("<a href='pca/", nodeName, "_PC1-2_S.html'", ">PCA-S</a>")
  PCA_Scor <-
    paste0("<a href='pca/", nodeName, "_PC1-2_L.html'", ">PCA-L</a>")

  # Link
  combined_path <- paste(HCA, PCA_Load, PCA_Scor, sep = " ")
  master_list[[node_pos]]$links <<- combined_path
}

pad_with_spaces <- function(nodeName) {
  nodePos <- get_node_position(nodeName)
  string <- master_list[[nodePos]]$links
  spaces <- paste(rep(" ", m - nchar(string)), collapse = "")
  master_list[[nodePos]]$links <<- paste0(string, spaces)
}

make_simple_PCA <-
  function(dataframe = df,
           nodeName,
           cex = 1.5,
           w = 11,
           h = 8) {
    p <- pca_based_on_node(dataframe = dataframe, nodeName)
    pdf(
      file = paste0(nodeName, "-simple.pdf"),
      width = w,
      height = h
    )
    plot(p$x[, 1:2], pch = 0, cex = cex)
    dev.off()
  }

# interactive
library(data.tree)
library(plotly)

listOfLists <- list(
  list_var = list(),
  list_num = list()
)

# initialize the list of nodes and add the first element
# for now, listName MUST == 'master_list'
# initiate_node_list <- function(listName, dataframe, PCA) {
#   assign(x = listName, value = list(), envir = .GlobalEnv)
master_list[[1]] <<-
    list(ID = "base",
         members = rownames(dataframe),
         parent = "",
         pathString = 'base',
         isLeaf = TRUE,
         var = PCA$sdev^2 / sum(PCA$sdev^2) * 100)
# }

get_tree <- function(LIST) {
  x <-
    data.frame(
      'pathString' = unlist(
        lapply(X = LIST,
               FUN = function(x) x$pathString)),
      'number' = unlist(
        lapply(X = LIST,
               FUN = function(x) length(x$members))),
      'expanded' = unlist(
        lapply(X = LIST,
               FUN = function(x) ifelse(x$isLeaf, "No", "Yes"))),
      'canBeExpanded' = unlist(
        lapply(X = LIST,
               FUN = function(x) ifelse((length(x$members) > 30 && x$isLeaf), "Yes", "No"))),
      'PC1_PC2' = unlist(
        lapply(X = LIST,
               FUN = function(x) round(x$var[1] + x$var[2], 2)))
    )
  x2 <- as.Node(x)
  return(x2)
}

display_tree <- function(LIST) {
  print(get_tree(LIST), 'number', 'expanded', 'canBeExpanded', 'PC1_PC2')
}

get_node_position <- function(nodeName) {
  n <- which(lapply(X = master_list, FUN = function(x) x$ID == nodeName) == 1)
  if (length(n) == 0) {
    return(-1)
  }
  else return(n)
}

unexplored_nodes <- function(nameOrNumber) {
  # returns a vector of node names or numbers
  if (identical(nameOrNumber, "name")) {
    v = unlist(
      lapply(X = master_list,
             FUN = function(x) {if(x$isLeaf) {x$ID}}
      ))
    return(v)
  }
  if (identical(nameOrNumber, "number")) {
    v = unlist(
      lapply(X = master_list,
             FUN = function(x) {if(x$isLeaf) {length(x$members)}}
      ))
    return(v)
  }
}

pca_based_on_membership <- function(name_list = NULL, scaled_data = NULL) {

  # if supplied with scaled data, skip the next block
  if (is.null(scaled_data)) {
    df <- remove_zeros(f.features[name_list, ])

    # scale -> make correlation matrix -> cluster ->
    # make dendrogram -> add labels
    df.s <- scale_pareto(df) # pareto scaled data
  } else {
    df.s <- scaled_data
  }

  df.pca <- prcomp(df.s, scale. = F, center = F)
  return(df.pca)
}

pca_based_on_node <- function(nodeName) {
  n <- get_node_position(nodeName)
  p <- pca_based_on_membership(name_list = master_list[[n]]$members)
  return(p)
}

explained_variance_of_node <-
  function(nameOfNode = NULL, position = NULL, scaled_data = NULL) {
    # returns a vector of explained variance for a given node; hopefully not all
    # three are null at any one time

    # retrieve node from list if name supplied
    if (!is.null(nameOfNode)) {
      n <- get_node_position(nameOfNode)
    } else {
      n <- position
    }

    # calculate pca of node based on its members if is supplied instead
    p <- pca_based_on_membership(master_list[[n]]$members, scaled_data)


    # the explained variance in a vector
    v <- p$sdev^2 / sum(p$sdev^2) * 100
    return(v)
  }

process_node <- function(id) {
  # finds the element in the list with the ID of id
  n <- get_node_position(id)

  cat(paste0('\n[1/4] found node at position: ', n))

  # node is then just an element from the master_list with the correct id
  node <- master_list[[n]]

  # Get dataframe and remove zeroes
  df <- remove_zeros(f.features[node$members, ])

  # Scale, get distance metric, cluster, and get dendrogram with labels
  df.s <- scale_pareto(df) # pareto scale the data
  df.cor <- cor(x = t(df.s),y = t(df.s)) # correlation matrix
  df.clust <- hclust(as.dist(1 - df.cor), method = "average") # clusters
  df.dend <- as.dendrogram(df.clust) # as dendrogram object
  labels(df.dend) <-
    rownames(df)[order.dendrogram(df.dend)] # labels
  cat('\n[2/4] selected and processed data and made dendrogram')

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
    list(ID = leftChildID,
         members = leftChildMembers,
         parent = node$ID,
         pathString = leftChildPathString,
         isLeaf = TRUE)
  cat(paste0('\n[3/4] made left child called: ', leftChildID))

  # right child [append a 1]
  rightChildID = paste0(node$ID, 1)
  rightChildMembers = names(which(membership == 2))
  rightChildPathString = paste(node$pathString, rightChildID, sep = "/")
  master_list[[new_node_position + 1]] <<-
    list(ID = rightChildID,
         members = rightChildMembers,
         parent = node$ID,
         pathString = rightChildPathString,
         isLeaf = TRUE)
  cat(paste0('\n[4/4] made right child called: ', rightChildID))

  # set isLeaf option of current node to FALSE
  master_list[[n]]$isLeaf <<- FALSE
}

start_gui <- function() {
  while (TRUE) {
    cat(
      '\nYou can:
      1. Type the name of a node to process, or
      2. Type "tree" to print the current tree structure, or
      3. Type "show" to list nodes that have not been expanded yet, or
      4. Type "exit" to end this prompt.'
    )
    answer <- readline(prompt = "--> ")
    answer <- str_to_lower(answer)

    # check if 'exit'
    if (identical(answer, 'exit')) {
      break
    }
    # check if 'tree'
    if (identical(answer, 'tree')) {
      display_tree()
      next
    }
    # check if 'show'
    if (identical(answer, 'show')) {
      cat('--------\n')
      print(cbind('name' = unexplored_nodes('name'),
                  'number of members' = unexplored_nodes('number')))
      cat('--------\n')
      next
    }

    # check if 'auto'
    if (identical(answer, 'auto')) {
      while (TRUE) {
        answer <- str_to_lower(readline(prompt = '--- type "v" for variance or "n" for number > '))
        if (identical(answer, 'v')) {
          answer <- readline(prompt = 'enter a minimum variance threshold > ')

        }
      }
    }
    # Now, we know that the answer isn't one of the other options, we
    # can query the list to get the position of the node
    n <- get_node_position(answer)

    # check if node name was typed incorrectly
    if (n == -1){
      cat("\n Node name not found. Please try again. \n")
      next
    }
    # check if node is a leaf; leaves are nodes that have not been processed yet
    if (!master_list[[n]]$isLeaf) {
      cat('\nThat node has already been processed. Try a different node.\n')
      next
    } else {
      process_node(answer)
    }
  }
}

auto_process <- function(numOrVar = "num", N = 30) {
  # check if 'N' is a number
  if (!is.numeric(N)) {
    return("Not a number!")
  }

  # default assumes "num" but other option is "var" for a variance cutoff
  if (identical(numOrVar, "num")) {
    if (N %% 1 != 0) {
      return("Not a whole number!")
    } # must be a whole number for strain cutoff

    # if the tests pass, then we know to process based on a number cutoff
    while(TRUE) {
      a <- unexplored_nodes('name')
      b <- unexplored_nodes('number')

      # hopefully, the two vectors are the same length
      a <- a[b > N]

      # if no node meets the above criterion, a is a 0 length character vector
      if (identical(a, character(0))) {
        return("Finished auto-generating tree based on number.")
      }

      # process all nodes present in a
      for (i in a) {
        cat('\n=====')
        process_node(i)
        cat('=====\nProcessed!\n')
      }
    }

  } else if (identical(numOrVar, "var")) {
    N <- round(N, 2) # round the input value to two decimals

    while(TRUE) {
      a <- unexplored_nodes('name') # get list of unexpanded nodes
      b <- c()

      for (i in a) {
        v <- explained_variance_of_node(i)
        n <- get_node_position(i)
        master_list[[n]]$var <<- v
        b <- c(b, round(v[1]+v[2], 2))
      }

      # filter array based on the cutoff variance
      a <- a[b < N]

      if (identical(a, character(0))) {
        return("Finished auto-generating tree based on variance.")
      }

      for (i in a) {
        cat('\n=====')
        process_node(i)
        cat('=====\nProcessed!\n')
      }
    }

  } else {
    return("Please enter either 'num' or 'var'.")
  }
}

make_pca_plot <- function(nodeName, axis1 = 1, axis2 = 2) {
  p <- pca_based_on_node(nodeName)

  plt_scores <-
    plot_ly(data = as.data.frame(p$x[, c(axis1, axis2)]),
            x = as.formula(paste0("~PC", axis1)),
            y = as.formula(paste0("~PC", axis2)),
            # x = paste0("~PC",axis1), y = paste0("~PC", axis2),
            marker = list(size = 10, color = 'rgba(255, 182, 193, 0.9)',
                          line = list(color = 'rgba(152, 0, 0, 0.8)',
                                      width = 2)),
            hoverinfo = 'text', text = ~rownames(p$x)) %>%
    layout(title = paste0('Scores Plot: PC',axis1,' v PC', axis2))

  plt_loadings <-
    plot_ly(data = as.data.frame(p$rotation[, c(axis1, axis2)]),
            x = as.formula(paste0("~PC", axis1)),
            y = as.formula(paste0("~PC", axis2)),
            # x = paste0("~PC",plane1), y = paste0("~PC",plane2),
            marker = list(size = 10, color = 'rgba(184, 184, 255, 0.9)',
                          line = list(color = 'rgb(0, 0, 153, 0.8)',
                                      width = 2)),
            hoverinfo = 'text', text = ~rownames(p$rotation)) %>%
    layout(title = paste0('Loadings Plot: PC',axis1,' v PC', axis2))

  return(list('scores' = plt_scores, 'loadings' = plt_loadings))
}

make_pca_html <- function(nodeName, axis1=1, axis2=2) {
  p <- make_pca_plot(nodeName, axis1, axis2)
  htmlwidgets::saveWidget(p$scores,
                          paste0(nodeName, "PC", axis1, "-", axis2, "Scores.html"))
  htmlwidgets::saveWidget(p$loadings,
                          paste0(nodeName, "PC", axis1, "-", axis2, "Loadings.html"))
  return("done!")
}

make_dendrogram <- function(nodeName) {
  n <- get_node_position(nodeName)
  name_list <- master_list[[n]]$members

  df <- remove_zeros(f.features[name_list, ])
  df.s <- scale_pareto(df)
  df.cor <- cor(x = t(df.s),y = t(df.s)) #correlation matrix
  df.clust <- hclust(as.dist(1 - df.cor), method = "average") #clusters
  df.dend <- as.dendrogram(df.clust) #as dendrogram object
  labels(df.dend) <-
    rownames(df)[order.dendrogram(df.dend)] #label each leaf
  master_list[[n]]$dend <<- df.dend
  return(df.dend)
}

make_hca_plot_pdf <- function(nodeName, w=16, h=9, lwd=3) {
  n <- get_node_position(nodeName)
  d <- master_list[[n]]$dend
  number_of_members <- length(master_list[[n]]$members)

  if (is.null(d)) {
    d <- make_dendrogram(nodeName)
  }

  pdf(file = paste0(nodeName,"-", number_of_members, ".pdf"),
      width = w, height = h)
  par(bg = "#F0F0F0", fg = "#5e5e5e")
  # par(bg = "#d2d2d2", fg = "#6e6e6e")
  colors = c("#FF2700", '#008FD5', '#77AB43')
  # colors = c("#FF2800", "#FFdd00", "#00bb3f", "#4512ae") # red, yellow, green, blue(ish)
  # colors = c("#FED439", "#197EC0", "#91331F")
  d %>%
    set("branches_lwd", lwd) %>%
    # set("branches_col", colors[1]) %>%
    # Color branches according to their genus membership
    set(what = "by_labels_branches_col",
        value = fMicro[fMicro %in% get_leaves_attr(d, 'label')],
        TF_values = c(colors[1], Inf)) %>%
    set(what = "by_labels_branches_col",
        value = fSolwa[fSolwa %in% get_leaves_attr(d, 'label')],
        TF_values = c(colors[2], Inf)) %>%
    set(what = "by_labels_branches_col",
        value = fStrep[fStrep %in% get_leaves_attr(d, 'label')],
        TF_values = c(colors[3], Inf)) %>%
    # set(what = "leaves_col", value = "#4b4b4b") %>%
    # set(what = "leaves_pch", value = c(ifelse(
    #   get_leaves_attr(d, 'label') %in% get_leaves_attr(d[[1]], 'label'), 15, 17))) %>%
    # color_labels(dend = ., k = 2) %>%
    hang.dendrogram %>%
    # plot()
    plot(panel.first = {grid(col = '#FFFFFF', lty = 3, nx = NA, ny = NULL)})
  # legend(x = "bottomright", legend = c("Micro", "Solwa", "Strep", "Verr"),
  #        col = c(colors[1:3], "#6e6e6e"),
  #        lty = 1, lwd = 4, horiz = TRUE, cex = 2)
  dev.off()
}

find_strain <- function(strainName) {
  unlist(
    lapply(master_list,
           FUN = function(x) {
             if(length(which(x$members == strainName)) != 0) {
               x$ID
             }}
    ))
}

#### - ####
process_node('base')

sapply(unlist(lapply(master_list,
                     FUN=function(x) x$ID)),
       FUN = function(x) make_hca_plot_pdf(x))

pdf(file = "explainedVar.pdf", width = 16, height = 9)
par(bg = "#F0F0F0", fg = "#5e5e5e")
plot(master_list_var[[1]]$var[1:10], type='l', col = "#77AB43", lwd=4,
     ylab = 'Explained Variance', xlab = 'Principal Component',
     panel.first = {grid(col = '#5e5e5e', lty = 3, nx = NULL, ny = NULL)})
points(master_list_var[[1]]$var[1:10], pch = 1, col = 'black')
# lines(master_list[[1]]$var, type='l', col = "#77AB43", lwd=3)
# lines(master_list_var[[2]]$var[1:50], type='l', col = "#FF2700", lwd=4)
# lines(master_list_var[[3]]$var[1:50], type='l', col = "#008FD5", lwd=4)
dev.off()

#### - ####

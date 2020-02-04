#!/usr/bin/env Rscript

# Start of script
start_time <- Sys.time()

# Libraries
cat("\n", "------loading libraries-------", "\n")

suppressMessages(library(data.table)) # reading data
suppressMessages(library(stringr)) # parsing row names; regex
suppressMessages(library(dendextend)) # tree visualization
suppressMessages(library(data.tree)) # tree visualization 2
suppressMessages(library(plotly)) # interactive plots
suppressMessages(library(config)) # config file

cat("\n", "------loaded libraries-------", "\n")

# Variables
parameters <- config::get(file = "config_file.yaml")
source(file.path(
  parameters$input_folder_accessory_script,
  "accessory_functions.R"
))

cat("\n", "------reading data-------", "\n")

if (is.null(parameters$single_table) &&
    is.null(parameters$spectral_table)) {
  stop("Can't have both a single table and multiple files. Comment out one or
       the other secion in the config.yaml file")
}

if (is.null(parameters$single_table)) {
  df <- fread(
    input = parameters$spectral_table,
    header = F,
    data.table = F
  )
  dfrows <- fread(
    input = parameters$sample_names,
    header = F,
    data.table = F,
    sep = NULL
  )
  dfcolM <- fread(input = parameters$mass_table,
                  header = F,
                  data.table = F)
  dfcolT <- fread(input = parameters$time_table,
                  header = F,
                  data.table = F)

  # ~~~~~ Fix row and column names ~~~~~
  dfrows <-
    str_extract(string = dfrows[, 1], pattern = parameters$row_name_pattern)

  # check for duplicated rows
  duplicates <- duplicated(dfrows)
  if (any(duplicates)) {
    # if any duplicates are found
    df <- df[c(!duplicates),] # remove corresponding rows from data
    dfrows <- dfrows[c(!duplicates)] # remove duplicated samples
  }

  # # find columns that are completely zero and remove them
  # col_to_keep <- which(colSums(df) != 0)
  # df <- df[, col_to_keep]

  # combine mass and time into one string separated by underscore
  dfcol <- paste0(dfcolM, "_", dfcolT)
  # # There are fewer columns in the original dataset because of the removal step. Select only the columns common to both the original data and the column names.
  # dfcol <- dfcol[col_to_keep]

  colnames(df) <- dfcol
  df <- as.data.frame(df)
  rownames(df) <- dfrows
} else {
  df <- fread(
    input = parameters$single_table,
    header = F,
    data.table = F,
    na.strings = ""
  )
  if (anyNA(df)) {
    df[is.na(df)] <- 0
  }
  if (!is.null(parameters$col_to_remove)) {
    df[, c(parameters$col_to_remove)] <- NULL
  }
  if (parameters$transpose) { # if it needs transposition
    df <- as.data.frame(t(as.matrix(df))) # transpose it
  }

  dfrows <- as.character(df[2:nrow(df),1])
  dfcols <- as.character(unlist(df[1,2:ncol(df)]))
  df[, 1] <- NULL
  df <- df[2:nrow(df), ]

  df <- as.data.frame(sapply(df, function(x) as.numeric(as.character(x))))

  duplicates <- duplicated(dfrows)
  if (any(duplicates)) {
    # if any duplicates are found
    df <- df[c(!duplicates),] # remove corresponding rows from data
    dfrows <- dfrows[c(!duplicates)] # remove duplicated samples
  }

  rownames(df) <- dfrows
  colnames(df) <- dfcols
}

save_object(saveDirectory = parameters$save_folder, object = "df")
cat("\n", "------read data-------", "\n")
cat("\n", "------calculating pca, hca-------", "\n")

# ~~~~ scale -> distance -> cluster -> dendgrogram -> label ~~~~
df.s <- scale_pareto(df)
df.s <- remove_nas(df.s)
df.cor <- cor(x = t(df.s), y = t(df.s)) # correlation matrix
df.clust <- hclust(as.dist(1 - df.cor), method = "average") # clusters
df.dend <- as.dendrogram(df.clust) # as dendrogram object
labels(df.dend) <-
  rownames(df)[order.dendrogram(df.dend)] #label each leaf
df.pca <- prcomp(df.s, center = F, scale. = F) # PCA of all samples

cat("\n", "------calculated pca, hca-------", "\n")

master_list = list()
master_list[[1]] <-
  list(
    ID = "b",
    members = rownames(df),
    parent = "",
    pathString = 'b',
    isLeaf = TRUE,
    var = df.pca$sdev ^ 2 / sum(df.pca$sdev ^ 2) * 100
  )
save_object(
  saveDirectory = parameters$save_folder,
  object = "df.pca",
  objectName = "b_pca"
)

cat("\n", "------processing tree-------", "\n")

# ~~~~ make entire tree and generate report ~~~~
process_node(dataframe = df, id = 'b')
auto_process(
  dataframe = df,
  numOrVar = parameters$numOrVar,
  N = parameters$N_numOrVar,
  saveDirectory = parameters$save_folder
)

cat("\n", "------processed tree-------", "\n")

# For debugging purposes
# if (parameters$save_image) {
#   save_points_dir <- file.path(getwd(), "save_points")
#   if (!dir.exists(save_points_dir)) {
#     dir.create(save_points_dir)
#   }
#   save.image(file.path(save_points_dir, "save_point.RData"))
# }

# Setup Directories; hca; pca
output_folder_hca <-
  file.path(parameters$output_folder, parameters$output_folder_hca)
if (parameters$output_pca) {
  output_folder_pca <-
    file.path(parameters$output_folder, parameters$output_folder_pca)
  if (!dir.exists(output_folder_pca)) {
    dir.create(output_folder_pca, recursive = TRUE)
  }
}
output_folder_report <-
  file.path(parameters$output_folder)
if (!dir.exists(output_folder_hca)) {
  dir.create(output_folder_hca, recursive = TRUE)
}

# Get list of node names for later functions
nodeIDs <- unlist(lapply(master_list, `[[`, "ID"))

# taken from the calc palette from ggthemes
# https://github.com/jrnold/ggthemes/blob/master/R/calc.R
colors = c(
  "#004586",
  "#ff420e",
  "#ffd320",
  "#579d1c",
  "#7e0021",
  "#83caff",
  "#314004",
  "#aecf00",
  "#4b1f6f",
  "#ff950e",
  "#c5000b",
  "#0084d1"
)
save_object(saveDirectory = parameters$save_folder, object = "colors")
original_palette <- palette() # store original palette
palette(colors)

cat("\n", "------making HCA plots-------", "\n")

if (!is.null(parameters$metadata)) {
  cat("\n", "------metadata found, colored HCA!-------", "\n")
  metadata <- fread(
    input = parameters$metadata,
    sep = "\t",
    header = T,
    data.table = F
  )
  if (is.null(parameters$single_table)) {
    if (any(duplicates)) {
      metadata <- metadata[c(!duplicates),]
    }
  } else {
    cols <-
      colnames(metadata)[2:ncol(metadata)] # all cols except first one
    metadata <-
      add_color_column(metadata = metadata, column_names = cols)
    color_column_names <-
      colnames(metadata)[str_detect(string = colnames(metadata), pattern = "color")]
    save_object(saveDirectory = parameters$save_folder, object = "metadata")
    for (node in nodeIDs) {
      n <- get_node_position(node)
      if (length(master_list[[n]]$members) < 4) {
        print("Three or fewer members; skipping...")
        next
      } else {
        make_colored_hca_plot_pdf(
          dataframe = df,
          nodeID = node,
          outfile = output_folder_hca,
          metadata = metadata,
          color_column_names = color_column_names
        )
      }
    }
  }
} else {
  cat("\n", "------no metadata file found------", "\n")
  # make standard HCA for each node
  for (node in nodeIDs) {
    n <- get_node_position(node)
    if (length(master_list[[n]]$members) < 4) {
      print("Three or fewer members; skipping...")
      next
    } else {
      make_hca_plot_pdf(nodeName = node,
                        dataframe = df,
                        outfile = output_folder_hca)
    }
  }
  cat("\n", "-------made HCA plots-------", "\n")
}

# add combined_path
for (node in nodeIDs) {
  add_links_attribute(node)
}

# find max length of links
m = max(unlist(lapply(
  master_list,
  FUN = function(x)
    nchar(x$links)
)))

# fix links attribute with spaces
for (node in nodeIDs) {
  pad_with_spaces(node)
}

# fix var in list
for (node in nodeIDs) {
  n <- get_node_position(node)
  if (is.null(master_list[[n]]$var)) {
    master_list[[n]]$var <- 0
  }
}

TLNN <- two_letter_node_Names(N = length(master_list))
names(TLNN) <- nodeIDs

for (i in seq_along(master_list)) {
  master_list[[i]]$name <- TLNN[master_list[[i]]$ID]
}

# make master_list pathStrings shorter and more human-readable
for (i in seq_along(master_list)) {
  master_list[[i]]$pathStringID <- master_list[[i]]$pathString
  master_list[[i]]$pathString <-
    change_pathString(oldPathString = master_list[[i]]$pathStringID)
}

save_object(saveDirectory = parameters$save_folder, object = "master_list")
pca_objects <-
  list.files(
    path = file.path(parameters$save_folder),
    pattern = "pca",
    full.names = T
  )

# Make pca html for everything (pc1 vs pc2)
if (parameters$output_pca) {
  cat("\n", "-------making PCA plots-------", "\n")
  for (obj in pca_objects) {
    nodeID <- str_extract(string = obj, pattern = "b[01]*")
    n <- get_node_position(nodeID = nodeID)
    if (length(master_list[[n]]$members) < 3) {
      print("Two or fewer members; skipping...")
      next
    } else {
      pca <- readRDS(file = obj)
      make_pca_html(
        nodeID = nodeID,
        axis1 = 1,
        axis2 = 2,
        outfile = output_folder_pca,
        max_points_loadings = parameters$max_points_loadings
      )
    }
  }
  cat("\n", "-------made PCA plots-------", "\n")
}

# make the report
default_options <- sapply(
  X = c("width", "max.print"),
  FUN = function(x)
    getOption(x)
)
options(width = 10000, max.print = 99999)
sink(file = file.path(output_folder_report, "report.html"))
cat(x = "<html><head><meta charset='utf-8'><title>Report</title></head><body><pre>")
display_tree(LIST = master_list)
cat(x = "</pre></body></html>")
sink()
options(width = default_options[1],
        max.print = default_options[2])

cat("\n", "-------Generated report.html-------", "\n")

end_time <- Sys.time()

cat("\n",
    "Total time taken to run script: ",
    format(end_time - start_time),
    ".\n", sep = "")


#!/usr/bin/env Rscript

# Start of script
start_time <- Sys.time()

# Libraries
suppressMessages(library(data.table)) # reading data
suppressMessages(library(stringr)) # parsing row names; regex
suppressMessages(library(dendextend)) # tree visualization
suppressMessages(library(data.tree)) # tree visualization 2
suppressMessages(library(plotly)) # interactive plots
suppressMessages(library(config)) # config file

cat("\n------loaded libraries-------")

# Variables
parameters <- config::get(file = "config_file.yaml")
source(file.path(parameters$input_folder_accessory_script, "accessory_functions.R"))

# Read files
df <- fread(
  input = parameters$spectral_table,
  header = F,
  data.table = T
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
if (parameters$pattern_needed) {
  dfrows <-
    str_extract(string = dfrows[, 1], pattern = parameters$row_name_pattern)
}

# check for duplicated rows
duplicates <- duplicated(dfrows)
if (any(duplicates)) {
  # if any duplicates are found
  df <- df[c(!duplicates), ] # remove corresponding rows from data
  dfrows <- dfrows[c(!duplicates)] # remove duplicated samples
}

# find columns that are completely zero and remove them
col_to_delete <-
  which(colSums(df) == 0, arr.ind = TRUE) #returns a named int
if (length(col_to_delete) != 0) {
  # there are columns that are completely zero
  df[, (col_to_delete) := NULL]
}

# combine mass and time into one string separated by underscore
dfcol <- paste0(dfcolM, "_", dfcolT)

# There are fewer columns in the original dataset because of the zero removal step. Select only the columns common to both the original data and the column names.
if (length(col_to_delete) != 0) {
  dfcol <- dfcol[-c(col_to_delete)]
}
colnames(df) <- dfcol

df <- as.data.frame(df)
rownames(df) <- dfrows

cat("\n---------read data----------")

# ~~~~ scale -> distance -> cluster -> dendgrogram -> label ~~~~
df.s = scale_pareto(df)
df.cor = cor(x = t(df.s), y = t(df.s)) # correlation matrix
df.clust = hclust(as.dist(1 - df.cor), method = "average") # clusters
df.dend = as.dendrogram(df.clust) # as dendrogram object
labels(df.dend) <-
  rownames(df)[order.dendrogram(df.dend)] #label each leaf
df.pca <- prcomp(df.s, center = F, scale. = F) # PCA of all samples

cat("\n----calculated pca, hca-----")

# For debugging purposes
if (parameters$save_image) {
  save_points_dir <- file.path(getwd(), "save_points")
  if (!dir.exists(save_points_dir)) {
    dir.create(save_points_dir)
  }
  save.image(file.path(save_points_dir, "save_point.RData"))
}

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

# ~~~~ make entire tree and generate report ~~~~
process_node(dataframe = df, id = 'b')
auto_process(
  dataframe = df,
  numOrVar = parameters$numOrVar,
  N = parameters$N_numOrVar
)

cat("\n-------processed tree--------")

# For debugging purposes
if (parameters$save_image) {
  save_points_dir <- file.path(getwd(), "save_points")
  if (!dir.exists(save_points_dir)) {
    dir.create(save_points_dir)
  }
  save.image(file.path(save_points_dir, "save_point.RData"))
}

# Setup Directories; hca; pca
output_folder_hca <-
  file.path(parameters$output_folder, parameters$output_folder_hca)
output_folder_pca <-
  file.path(parameters$output_folder, parameters$output_folder_pca)
output_folder_report <-
  file.path(parameters$output_folder)

if (!dir.exists(output_folder_hca)) {
  dir.create(output_folder_hca, recursive = TRUE)
}
if (!dir.exists(output_folder_pca)) {
  dir.create(output_folder_pca, recursive = TRUE)
}

# Get list of node names for later functions
nodeNames <- unlist(lapply(master_list, `[[`, "ID"))

# Mondrian Palette
colors = c(
  "#0f7fbf",
  # 1. blue
  "#fce317",
  # 2. yellow
  "#f70f0f",
  # 3. red
  "#202332",
  # 4. black-purple
  "#8f8e78",
  # 5. gray-green
  "#f0f0f0",
  # 6. gray94 (light gray)
  "#5e5e5e",
  # 7. gray37 (medium gray)
  "#ffffff"
) # 8. white

# make HCA for each node
for (node in nodeNames) {
  n <- get_node_position(node)
  if (length(master_list[[n]]$members) < 4) {
    print("Three or fewer members; skipping...")
    next
  } else {
    make_hca_plot_pdf(
      nodeName = node,
      dataframe = df,
      outfile = output_folder_hca
    )
  }
}

cat("\n-------made HCA plots-------")

# add combined_path
for (node in nodeNames) {
  add_links_attribute(node)
}

# find max length of links
m = max(unlist(lapply(
  master_list,
  FUN = function(x)
    nchar(x$links)
)))

# fix links attribute with spaces
for (node in nodeNames) {
  pad_with_spaces(node)
}

# fix var in list
for (node in nodeNames) {
  n <- get_node_position(node)
  if (is.null(master_list[[n]]$var)) {
    master_list[[n]]$var <- 0
  }
}

# Make pca html for everything (pc1 vs pc2)
for (node in nodeNames) {
  n <- get_node_position(node)
  if (length(master_list[[n]]$members) < 3) {
    print("Two or fewer members; skipping...")
    next
  } else {
    make_pca_html(
      nodeName = node,
      dataframe = df,
      outfile = output_folder_pca,
      max_points_loadings = parameters$max_points_loadings
    )
  }
}
cat("\n-------made PCA plots-------")

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

cat("\n------Report Generated------")

end_time <- Sys.time()

cat("\nTotal time taken to run script:", format(end_time - start_time))

# # ~~~~ Visualize entire tree ~~~~
# # 1. using ape
# library(ape)
# phy <- as.phylo.Node(x = get_tree(master_list))
# pdf(file=file.path(getwd(), "tree.pdf"), width=11, height=8)
# plot(phy, label.offset=0.2, no.margin=T, show.node.label=T, adj=1)
# dev.off()

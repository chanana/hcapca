## Hierarchical clustering and sub-clustering
library(data.table)
library(stringr)
library(dendextend)

####Directory and data ####
setwd("~/programming/clear_brackish/") #set working directory
source("~/programming/commonLib/read_data.R")
source("~/Programming/commonLib/edit_data.R")
source("~/Programming/commonLib/plot_data.R")
source("~/Programming/commonLib/NTBase.R")
bucket_table_name = "../spectral_tables/Brackish-Clear.txt"

f = get_spectral_file(bucket_table_name)
f[c(!duplicated(x = str_extract(string = f[,1], pattern = "[ABC][0-9]{3,4}"))), ] -> f

# # this step averages out any duplicate columns; must happen before any kind of transformation is done to the dataframe
# f <- as.data.frame(sapply(X = unique(names(f)), FUN = function(x) rowMeans(f[names(f) == x])))

remove_zeros(df = f[,-1]) -> f.features
f.features = cleanup_cols(dataframe = f.features, t = "min")
f.features = cleanup_rows(dataframe = f.features, dirty_rownames = f[, 1],
                          pattern = "[ABC][0-9]{3,4}", replicates = 1)

fMicro = rownames(f.features)[1:105]
fSolwa = rownames(f.features)[106:128]
fStrep = rownames(f.features)[129:186]
fVerru = rownames(f.features)[187:239]

#### scale > PCA > distance > cluster > dendgrogram > label ####
temp.s = scale_pareto(f.features)
temp.cor = cor(x = t(temp.s),y = t(temp.s)) #correlation matrix
temp.clust = hclust(as.dist(1 - temp.cor), method = "average") #clusters
f.dend = as.dendrogram(temp.clust) #as dendrogram object
labels(f.dend) <-
  rownames(f.features)[order.dendrogram(f.dend)] #label each leaf
temp.pca <- prcomp(temp.s, center = F, scale. = F)

#### FOR LOOP ####
#the 'i'th element of the list has 'i' elements. If we cut the cluster at a height of 10, then there are 10 sub-clusters formed and each of them needs to have a PCA done on them.
n = 2 #Can be set by the user
a = vector(mode = "list", length = n) #stores the cutoffs. if the cluster was cut into 'i' subclusters, it is in the 'i'th element of the list.
lis = list()
for (i in 2:n) {
  lis[[i]] = vector(mode = "list", length = i)
} #creates a list where the 'i'th element has 'i' elements (for PCAs)
heights = heights_per_k.dendrogram(dend = f.dend)
for (i in 2:n) {
  a[[i]] = cutree_1k.dendrogram(dend = f.dend, k = i, dend_heights_per_k = heights) #cut the tree into 'i' sub-clusters
  colors = rev(rainbow_hcl(i)) #colors for sub clusters
  filename = paste0("./", bucket_table_name, "-A", i, ".tiff") #figure out the filename
  tiff(
    filename = filename,
    width = 3 * i,
    height = 9,
    units = "in",
    res = 150
  ) #open a tiff
  layout.2(i) # define a layout
  plot(color_branches(f.dend, k = i)) #plot entire dendrogram with colors
  for (j in 1:i) {
    if ((table(a[[i]])[j] == 1) || (table(a[[i]])[j] == 2)) {
      plot.text("Two or fewer elements. No Scores", col = colors[j])
      plot.text("Two or fewer elements. No Loadings", col = colors[j])
      print(paste(
        "Two or fewer elements in cluster. Skipping", i, j, "...",sep = " "
      ))
      next
    }
    b = f.features[a[[i]] == j, ] #select only the strains from subcluster j from original bucket table
    b = remove_zeros(b) #remove columns with zeros
    b = scale_pareto(b) #pareto scale it
    lis[[i]][[j]] = prcomp(x = b,
                           scale. = F,
                           center = F) #calculate PCA for sub-cluster
    PCA = lis[[i]][[j]] #so that it's easier to refer to the PCA for that sub-cluster
    plot.1(PCA$x[, 1], PCA$x[, 2], pch = 21, bg = colors[j]) #plot scores
    text(PCA$x[, 1], PCA$x[, 2], labels = rownames(PCA$x), pos = 2) #label points
    plot.1(PCA$rotation[, 1],
           PCA$rotation[, 2],
           pch = 22,
           bg = colors[j]) #plot loadings
    print(paste("finished", i, j, sep = " "))
  }
  dev.off() #save tiff
  print(paste("saved", filename, sep = " "))
} #Actual loop to calculate the PCA of each sub-cluster
#####################################################################################################
## Draws a dark and/or white image of the entire dendrogram in hung/non-hung form
# labels_colors(dend = f.dend) <- ifelse(grepl(x = labels(f.dend), pattern = "MH"), 2, 3)
color_hue <- function(n) {
  hues = seq(from = 15, to = 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
# tiff(filename = paste0("bgMedGreyfgDarkGrey", ".tiff"), width = 36, height = 9, units = "in", res = 300)
pdf(file = "completelyDifferent9.pdf", width = 36, height = 9)
# par(bg = "#F0F0F0", fg = "#5e5e5e")
par(bg = "#d2d2d2", fg = "#6e6e6e")
colors = c("#FF2700", '#008FD5', '#77AB43')
# colors = c("#FF2800", "#FFdd00", "#00bb3f", "#4512ae") # red, yellow, green, blue(ish)
# colors = c("#FED439", "#197EC0", "#91331F")
gram <- f.dend %>%
  # set("branches_k_color", clusters = a[[2]][order.dendrogram(f.dend)], col = color_hue(2)) %>%
  # branches_color(dend = ., col = c("#FF2700", '#008FD5', '#77AB43'), clusters = a[[3]][order.dendrogram(f.dend)]) %>%
  set("branches_lwd", 3) %>%
  # set("branches_col", colors[4]) %>%
  set(what = "by_labels_branches_col", value = fMicro, TF_values = c(colors[1], Inf)) %>%
  set(what = "by_labels_branches_col", value = fSolwa, TF_values = c(colors[2], Inf)) %>%
  set(what = "by_labels_branches_col", value = fStrep, TF_values = c(colors[3], Inf)) %>%
  set(what = "leaves_col", value = "#4b4b4b") %>%
  set(what = "leaves_pch", value = c(ifelse(a[[2]][order.dendrogram(f.dend)]==1,15,17))) %>%
  # color_labels(dend = ., k = 2) %>%
  hang.dendrogram %>%
  # plot()
  plot(panel.first = {grid(col = '#FFFFFF', lty = 3, nx = NA, ny = NULL)})
legend(x = "bottomright", legend = c("Micro", "Solwa", "Strep", "Verr"), col = c(colors[1:3], "#6e6e6e"), lty = 1, lwd = 4, horiz = TRUE, cex = 2)
dev.off()
# par(bg = 'black',
#     col = 'white',
#     col.axis = "white",
#     col.lab = "white",
#     col.main = "white",
#     col.sub = "white",
#     fg = "white"
# ) # changes background to black and plotting color to white

tiff(
  filename = "Dendrogram-3-white.tiff",
  width = 16,
  height = 9,
  units = "in",
  res = 300
) #open a tiff
# par(bg = 'black',
#     col = 'white',
#     col.axis = "white",
#     col.lab = "white",
#     col.main = "white",
#     col.sub = "white",
#     fg = "white"
#     ) # changes background to black and plotting color to white
plot(color_branches(f.dend, k = number_of_colors, col = colors_for_dend))
dev.off()
###############################################################################
### PCA of the sub clusters with antibase ###
FLAG=1
if (FLAG == 1) {
  # Read antibase
  antibase <-
    read.csv("~/programming/commonLib/antibase_tableout.csv",
             stringsAsFactors = FALSE)
  # Take exact mass column
  antibase.exactmass = antibase$StructCalc
  # Cleanup: finds the first space and deletes it and
  # everything after it leaving only the exact mass.
  antibase.exactmass = str_replace(string = antibase.exactmass,
                                   pattern = " .*",
                                   replacement = "")
  # Make it of numeric type. This causes
  # an 'NAs introduced by coercion' warning. This is ok because we want
  # all non numeric values to be NA and can convert them to 0 later
  antibase.exactmass = as.numeric(antibase.exactmass)
  antibase.exactmass[which(is.na(antibase.exactmass))] = 0
  # Fill in blanks in the 'Names' column
  antibase$Name[which(antibase$Name == "")] = "NO_NAME"
}
# Constants ----
if (FLAG == 1) {
    ppm = 4
    PROTON = 1.6726231 / 1.6605402 # mass in kg to mass in u
    ELECTRON = (9.1093897 / 1.6605402) / 10000
    SODIUM = 22.989768
    SODIUM_Plus = SODIUM - ELECTRON
    }
# PoPCAR ----
i = 2# we only want to do it on when there are 2 sub clusters
colors = rev(rainbow_hcl(i)) #colors for sub clusters
for (j in 1:i) {
  i = 2
  if ((table(a[[i]])[j] == 1) || (table(a[[i]])[j] == 2)) {
    print(paste("Two or fewer elements in cluster. Skipping", i, j, "...", sep = " "))
    next
  }

  # Make separate directory for this sub-cluster
  subClusterDirName = paste("./B", i, "-", j, sep = "")
  if (!dir.exists(subClusterDirName)){
    dir.create(subClusterDirName)
  }

  PCA = lis[[i]][[j]] #so that it's easier to refer to the PCA for that sub-cluster
  filename = paste0("./B", i, "-", j, ".tiff") #figure out the filename
  ## Draw the scores (+labels) and loadings first as an overview
  tiff(
    filename = filename,
    width = 14.22,
    height = 8,
    units = "in",
    res = 300
  )
  par(mfrow = c(1, 2))
  plot(
    PCA$x,
    col = "black",
    bg = "cornflowerblue",
    pch = 21,
    main = "Scores",
    cex = 2
  )
  text(
    PCA$x[, 1],
    PCA$x[, 2],
    labels = rownames(PCA$x),
    pos = 2,
    bg = "black"
  )
  plot(
    PCA$rotation,
    col = "black",
    bg = "cornflowerblue",
    pch = 21,
    main = "Loadings"
  )
  dev.off()
  print(paste("finished", i, j, sep = " "))

  b = f.features[a[[i]] == j, ] #select only the strains from subcluster j from original bucket table
  b = remove_zeros(b) #remove columns with zeros

  ### Highest PCs
  fd.highestPCs = as.data.frame(
    apply(X = PCA$x,
          MARGIN = 1,
          FUN = function(y) order(abs(y), decreasing=TRUE)
  ))
  # fd.highestPCs = fd.highestPCs[, which(str_detect(string = colnames(fd.highestPCs),
  #                                                  pattern = ".ix"))]
  # colnames(fd.highestPCs) = str_replace(
  #   string = colnames(fd.highestPCs),
  #   pattern = ".ix",
  #   replacement = ""
  # )

  ### Get unique masses
  fd.unique = apply(
    X = b,
    MARGIN = 2,
    FUN = function(x)
      different_function(x, b)
  )
  ## Fish out the -1s and return everything else's column index
  filter_uniquemass = as.integer(which(fd.unique != -1))

  if (length(filter_uniquemass) == 0) {
    print(paste("No unique masses. Skipping", i, j, "...", sep = " "))
    next
  }
  # for now we simply take the masses which are unique to a strain from  the bucket table.
  buckets_unique = b[, filter_uniquemass]

  for (r in 1:(dim(b)[1])) {
    d = PCA$rotation[, fd.highestPCs[1:2, r]] #table containing PC planes of the strain in question
    d = as.data.frame(add_euclidean_distance(d)) #add euclidean distance
    d$m = str_replace(string = rownames(d),
                      pattern = ".*_",
                      replacement = "") # add column of masses to serve as a key
    if (sum(buckets_unique[r, ]) == 0) {
      print(paste("No unique masses. Skipping", rownames(buckets_unique)[r], "...", sep = " "))
      next
    }
    z = as.data.frame(t(buckets_unique[r,])) # masses for that strain
    z$rt = str_replace(string = rownames(z),
                       pattern = "_.*",
                       replacement = "") # add a time column
    z$m = str_replace(string = rownames(z),
                      pattern = ".*_",
                      replacement = "") # Add mass column to serve as a key
    z = as.data.frame(z[z != 0,]) # remove zeroes - only unique strain masses left
    zd = merge(x = z, y = d, by.x = 'm', by.y = 'm', sort = F) # merge on keys
    zd = add_euclidean_distance(dataframe = zd,
                                x = zd[, 4],
                                y = zd[, 5])
    zd = zd[order(zd$ed, decreasing = T),] # sort by Euclidean distance
    colnames(zd)[2] = "Intensity"
    zd = zd[, c(3, 1, 2, 4, 5, 6)]

    ## DRAW FIGURE!
    DrawFigures(
      filename = paste(
        subClusterDirName, "/", rownames(PCA$x)[r], ".tiff", sep = ""
        ),
      x = r,
      fd.highestPCs = fd.highestPCs)
    if (FLAG == 1) {
      # Now search for this mass list in antibase, and for each hit,
      # report the name of the hit. Pretty sure I don't need the next three lines.
      # mzrt = read.table(text = rownames(zd),
      #                   sep = "_",
      #                   colClasses = "numeric")
      # z$rt = mzrt$V1
      # z$mz = mzrt$V2
      # Column for putting in (later) which masses are NOT in AntiBase
      zd$Unique = NA
      # Search in AntiBase
      match_mass = lapply(as.numeric(zd$m), function(x)
        filter_M(x, antibase.exactmass, ppm))
      match_hydrogen = lapply(as.numeric(zd$m), function(x)
        filter_H(x, antibase.exactmass, ppm))
      match_sodium = lapply(as.numeric(zd$m), function(x)
        filter_Na(x, antibase.exactmass, ppm))

      txtFileName = paste(rownames(buckets_unique)[r], colnames(zd)[4], "vs", colnames(zd)[5], sep = " ")
      txtFileNameDir = paste(subClusterDirName, "/", txtFileName, ".txt", sep="")
      # Header
      text_block_1 =
        c(
          txtFileName,
          "=================================================="
        )
      write(text_block_1, file = txtFileNameDir)

      for (p in 1:dim(zd)[1]) {
        # Sub-header for that particular mz/rt pair
        text_block_2 =
          c(
            paste(
              paste(zd$rt[p], "min -", zd$m[p], "m/z", sep = " "),
              round(zd$Intensity[p], 2),
              sep = ", ")
          )
        write(text_block_2, append=T, file = txtFileNameDir)

        # Matches
        if (length(match_mass[[p]]) != 0) {
          text_block_3 =
            c(
              " M matches: ",
              paste(antibase$Name[match_mass[[p]]], collapse = '; ')
            )
          write(text_block_3, append=T, file = txtFileNameDir)
        }
        if (length(match_hydrogen[[p]]) != 0) {
          text_block_4 =
            c(
              " M-H matches: ",
              paste(antibase$Name[match_hydrogen[[p]]], collapse = '; ')
            )
          write(text_block_4, append=T, file = txtFileNameDir)
        }
        if (length(match_sodium[[p]]) != 0) {
          text_block_5 =
            c(
              " M-Na matches: ",
              paste(antibase$Name[match_sodium[[p]]], collapse = '; ')
            )
          write(text_block_5, append=T, file = txtFileNameDir)
        }
        # Separator between each mz/rt pair
        write(c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"), append = T, file = txtFileNameDir)

        if (length(match_mass[[p]]) == 0 &&
            length(match_hydrogen[[p]]) == 0 &&
            length(match_sodium[[p]]) == 0) {
          zd$Unique[p] = 1
        }
      }
      text_block_6 =
        paste(sum(zd$Unique, na.rm = T), " out of ", dim(zd)[1], " are unique.", collapse = "")
      write(text_block_6, append=T, file = txtFileNameDir)
    }
  }
}

pca <- function(parent_dataframe, names_vector, subClusterDirName) {
  # names_vector is the list of strains to be selected from parent_dataframe

  subClusterDirName = paste0("./", subClusterDirName)
  if (!dir.exists(subClusterDirName)){
    dir.create(subClusterDirName)
  }

  # subset dataframe for only desired membership; simultaneously
  # remove zeros (avoids errors in PCA)
  df <- remove_zeros(
    parent_dataframe[rownames(parent_dataframe) %in% names_vector, ]
  )

  df <- df[, -c(grep(pattern = "\\.[0-9]$", x = colnames(df)))]

  # scale -> PCA
  df.s <- scale_pareto(df) # pareto scaled data
  df.pca <- prcomp(df.s, scale. = FALSE, center = FALSE)

  # Highest PCs
  df.highestPCs = as.data.frame(
    apply(X = df.pca$x, MARGIN = 1,
          FUN = function(x) order(abs(x), decreasing=TRUE))
  )

  for (r in 1:(dim(df)[1])) {
    # table containing PC planes of the strain in question
    d <- df.pca$rotation[, df.highestPCs[1:2, r]]

    # add euclidean distance column named 'ed'
    d <- as.data.frame(add_euclidean_distance(d))

    # add column of mass and retention
    d$m <- as.numeric(sub(pattern = "^.*_", replacement = "", x = rownames(d)))
    d$rt <- as.numeric(sub(pattern = "_.*", replacement = "", x = rownames(d)))

    # order by decreasing euclidean distance
    d <- d[order(d$ed, decreasing = TRUE), ]

    ## DRAW FIGURE!
    DrawFigures(
      filename = paste(
        subClusterDirName, "/", rownames(df.pca$x)[r], ".tiff", sep = ""),
      r = r, df.highestPCs = df.highestPCs, PCA = df.pca)
    if (FLAG == 1) {
      # Now search for this mass list in antibase, and for each hit,
      # report the name of the hit. Pretty sure I don't need the next three lines.
      # Column for putting in (later) which masses are NOT in AntiBase
      d$Unique = NA
      # Search in AntiBase
      match_mass = lapply(as.numeric(d$m), function(x)
        filter_M(x, antibase.exactmass, ppm))
      match_hydrogen = lapply(as.numeric(d$m), function(x)
        filter_H(x, antibase.exactmass, ppm))
      match_sodium = lapply(as.numeric(d$m), function(x)
        filter_Na(x, antibase.exactmass, ppm))

      txtFileName = paste(rownames(df)[r], colnames(d)[1], "vs", colnames(d)[2], sep = " ")
      txtFileNameDir = paste(subClusterDirName, "/", txtFileName, ".txt", sep="")
      # Header
      text_block_1 =
        c(
          txtFileName,
          "=================================================="
        )
      write(text_block_1, file = txtFileNameDir)

      for (p in 1:dim(d)[1]) {
        # Sub-header for that particular mz/rt pair
        text_block_2 = paste(d$rt[p], "min -", d$m[p], "m/z", sep = " ")
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
            length(match_sodium[[p]]) == 0) {d$Unique[p] = 1}
      }
      text_block_6 =
        paste(sum(d$Unique, na.rm = T), " out of ", dim(d)[1], " are unique.", collapse = "")
      write(text_block_6, append=T, file = txtFileNameDir)
    }
  }
}
lapply(trees, function(x) list(x$name, table(x$membership)))
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[2]]$membership == 2)),
    subClusterDirName = "L11-2")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[3]]$membership == 2)),
    subClusterDirName = "L111-2")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[4]]$membership == 1)),
    subClusterDirName = "L1111-1")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[4]]$membership == 2)),
    subClusterDirName = "L1111-2")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[5]]$membership == 2)),
    subClusterDirName = "L12-2")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[6]]$membership == 1)),
    subClusterDirName = "L121-1")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[6]]$membership == 2)),
    subClusterDirName = "L121-2")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[7]]$membership == 2)),
    subClusterDirName = "L2-2")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[8]]$membership == 1)),
    subClusterDirName = "L21-1")
pca(parent_dataframe = f.features,
    names_vector = names(which(trees[[8]]$membership == 2)),
    subClusterDirName = "L21-2")

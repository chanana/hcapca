library(ggplot2)
make_PDF_gg_text <- function(PCA, w, h, name) {
  # PCA = lis[[total_clusters]][[cluster]]
  e = as.data.frame(PCA$x[, 1:2])
  # d = as.data.frame(PCA$rotation[, 1:2])
  # d = add_euclidean_distance(d)
  # d = d[order(d$ed, decreasing = TRUE), ]
  # d = d[1:10000, ]
  #
  ggplot(e, aes(x = PC1, y = PC2), environment = environment()) +
    geom_point(size = 4, alpha = 0.5) +
    geom_text(label = rownames(e))#, nudge_x = 30, nudge_y = 30)#, vjust = "inward", hjust = "inward")
  ggsave(filename = paste0(name, ".eps"), width = w, height = h, device = cairo_ps, dpi = 'print', units = 'in')
  # ggplot(d, aes(x = PC1, y = PC2), environment = environment()) + geom_point(size = 4, alpha = 0.5, colour = color)
  # ggsave(filename = paste0(total_clusters, "-", cluster, "-Loadings", ".eps"), width = w, height = h, device = cairo_ps)#, dpi = 'print', units = 'in')
}

# df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(a[[2]]==1))), ])
# df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(temp.membership==1))), ])
# df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(temp.membership==1))), ])
# df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(temp.membership==1))), ])
# df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(trees[[1]]$membership==2))), ])
# df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(trees[[5]]$membership==1))), ])
# df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(a[[2]]==2))), ])
df.temp <- remove_zeros(f.features[(rownames(f.features) %in% names(which(temp.membership==1))), ])

temp.s = scale_pareto(df.temp)
temp.cor = cor(x = t(temp.s),y = t(temp.s)) #correlation matrix
temp.clust = hclust(as.dist(1 - temp.cor), method = "average") #clusters
temp.dend = as.dendrogram(temp.clust) #as dendrogram object
labels(temp.dend) <-
  rownames(df.temp)[order.dendrogram(temp.dend)] #label each leaf
temp.membership = cutree_1k.dendrogram(dend = temp.dend, k = 2)
table(temp.membership)
temp.dend <- temp.dend %>%
  set("branches_lwd", 2) %>%
  set(what = "by_labels_branches_col", value = fMicro, TF_values = c(1, Inf)) %>%
  set(what = "by_labels_branches_col", value = fSolwa, TF_values = c(2, Inf)) %>%
  set(what = "by_labels_branches_col", value = fStrep, TF_values = c(3, Inf)) %>%
  set(what = "leaves_pch", value = 19) %>%
  set(what = "leaves_col", value = c(temp.membership[order.dendrogram(temp.dend)]))

temp.dend %>% hang.dendrogram %>% plot(panel.first = {grid(col = '#FFFFFF', lty = 3)})
trees = list.append(trees, list(name = "L2", dendrogram = temp.dend, membership = temp.membership))
# trees[[5]] = list(name = "L12", dendrogram = temp.dend, membership = temp.membership)

lapply(trees, function(x) table(x$membership))

# plot_data
layout.2 = function(x) {
  mat = rbind(rep(1, x), rev(seq(
    from = 2,
    to = (2 * x),
    by = 2
  )), rev(seq(
    from = 3,
    to = (2 * x + 1),
    by = 2
  )))
  layout(mat = mat)
}

plot.1 = function(x, y, ...) {
  m = max(abs(x), abs(y))
  plot(
    x,
    y,
    col = "black",
    cex = 1.5,
    xlim = c(-m, m),
    ylim = c(-m, m),
    ...
  )
  abline(v = 0, h = 0, lty = 3)
} #plots square graph, ,black border, 1.5x size

plot.text = function(t, ...) {
  plot(
    c(0, 1),
    c(0, 1),
    ann = F,
    type = 'n',
    xaxt = 'n',
    yaxt = 'n'
  )
  text(x = 0.5, y = 0.5, paste(t), ...)
} #Plot a text box in place of a graph with 't' as text

DrawFigures <-
  # Draws the scores and loadings based on where the strain stands out.
  function(filename, r, df.highestPCs, PCA) {
    y = df.highestPCs[1:2, r]
    tiff(
      filename = filename,
      height = 6,
      width = 12,
      units = "in",
      res = 300
    )
    par(mfrow = c(1, 2))
    plot(
      PCA$x[, y],
      pch = 21,
      col = "black",
      bg = "cornflowerblue",
      main = "Scores"
    )
    points(
      x = PCA$x[r, y[1]],
      y = PCA$x[r, y[2]],
      pch = 21,
      col = "black",
      bg = "firebrick1"
    )
    # text(
    #     fd.pca$x[x, y],
    #     col = "orange",
    #     labels = rownames(fd.pca$x)[x],
    #     pos = 1,
    #     font = 2
    # )
    plot(
      PCA$rotation[, y],
      pch = 21,
      col = "black",
      bg = "cornflowerblue",
      main = "Loadings"
    )
    # points(zd[, 4:5],
    #        col = "black",
    #        bg = "firebrick1",
    #        pch = 21)
    dev.off()
  }

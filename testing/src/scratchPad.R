library(data.tree)

acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")

print(acme)
acme$Accounting$`<a href="legend.txt">New Software</a>`$cost <- 1000000
acme$Accounting$`New Accounting Standards`$cost <- 500000
acme$Research$`New Product Line`$cost <- 2000000
acme$Research$`New Labs`$cost <- 750000
acme$IT$Outsource$cost <- 400000
acme$IT$`Go agile`$cost <- 250000
acme$IT$`Switch to R`$cost <- 50000

acme$Accounting$`New Software`$p <- 0.5
acme$Accounting$`New Accounting Standards`$p <- 0.75
acme$Research$`New Product Line`$p <- 0.25
acme$Research$`New Labs`$p <- 0.9
acme$IT$Outsource$p <- 0.2
acme$IT$`Go agile`$p <- 0.05
acme$IT$`Switch to R`$p <- 1
print(acme, "cost", "p")

li <- list()
li[[1]] <- list(
  "name"="a",
  "number"=12,
  "path"="a",
  "link"="<a href='error404'>error404</a>",
  "isLeaf"=FALSE
)
li[[2]] <- list(
  "name"="a.a",
  "number"=34,
  "path"="a/a.a",
  "link"="<a href='corrected.tiff'>correctedTiff</a>",
  "isLeaf"=TRUE
)
li[[3]] <- list(
  "name"="b.a",
  "number"=34,
  "path"="b/b.a",
  "link"="<a href='merged.csv'>mergedCSV</a>",
  "isLeaf"=TRUE
)
li[[4]] <- list(
  "name"="a.b",
  "number"=56,
  "path"="a/a.b",
  "link"="<a href='legend.txt'>legendTxt</a>",
  "isLeaf"=TRUE
)
li[[5]] <- list(
  "name"="b.b",
  "number"=78,
  "path"="b/b.b",
  "link"="<a href='base2.pdf'>base2PDF</a>",
  "isLeaf"=TRUE
)

tree2df <- function(lis) {
  x<-
    data.frame(
      'pathString' = unlist(
        lapply(X = lis,
               FUN = function(x) x$path)),
      'number' = unlist(
        lapply(X = lis,
               FUN = function(x) x$number)),
      'link' = unlist(
        lapply(X = lis,
               FUN = function(x) x$link))
      )
  return(x)
}
get_tree <- function(LIST) {
  x <- as.Node(tree2df(LIST))
  return(x)
}
print(get_tree(li), "number", "link")

unlist(lapply(X = li, FUN= function(x) x$isLeaf))

setwd("~/programming/icbg-everything/")

options(width = 10000, max.print = 99999)
sink(file = "report-Big.html")
cat(x = "<html><head><meta charset='utf-8'><title>Report</title>
</head><body><pre>") # ,file = "test.html"
# write.table(x=ToDataFrameTree(get_tree(master_list), 'number', 'expanded', 'canBeExpanded', 'PC1_PC2', 'linkToHCA', 'linkToPCAS', 'linkToPCAL'), file = "test.html", append=T, row.names = F, col.names = T, quote=F)
display_tree(master_list)
cat(x = "</pre></body></html>")
# , file="test.html", append=T)
sink()

sapply(
  X = master_list,
  FUN = function(x) ifelse(
    x$isLeaf,
    {
      pdf(file=paste0(x$ID, "-simple.pdf"), width=11, height=8)
      plot(pca_based_on_node(x$ID)$x[,1:2], pch=0)
      dev.off()
      cat(x$ID)
    },
    cat(" Not Found ")
)


library(networkD3)
saveNetwork(simpleNetwork((ToDataFrameNetwork(get_tree))), file="network.html")


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

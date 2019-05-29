# Instructions

Congrautulations! You successfully ran `hcapca` on your data. Now you can look at it interactively. This brief tutorial will take you through how to interpret the tabs on the left.

## 1. HCA insert icon
Hierarchical Clustering Analysis (HCA) of your data was done repeatedly until the given variance cutoff was met. This means we started with your entire dataset, clustered it, broke it into the top two clusters, and repeated that process until that variance cutoff was reached.

#### 1.1 Overall Tree
The following is an interactive tree that starts out at base node `b` and breaks up into `b0` and `b1`. These nodes further break up until the variance cutoff is reached for that node.
Nodes with filled circles can be expanded further by clicking on them.

#### 1.2 Dendrogram of Selected Node
Select a node from the drop down menu in order to view the samples present in it.

## 2. PCA insert icon
Principal Component Analysis (PCA) of each terminal node (nodes with no more child nodes) was performed to generate a model of the variance at that node. This allows you to determine which samples and molecules contribute to the overall variance the most.

#### 2.1 Parameters
Once you have selected a node, three more menus will appear asking you to select the `X` and `Y` axes as well as the `total number of molecules` you'd like to see on the loadings plot. We recommend rendering no more than 10000 molecules since it tends to slow things down *considerably*. Each axis represents a different combination of factors responsible for the overall variance in the samples present in the selected node. For a further discussion on PCA, we encourage you to read [this excellent resource](https://doi.org/10.1038/nmeth.4346).

#### 2.2 Scores Plot
Upon selecting your parameters and clicking on `Plot`, you will be presented with three plots, the first of which is the `Scores` plot. This shows you which samples are responsible for the variance seen in your data.

#### 2.3 Loadings Plot
Geometrically related to the `Scores` plot is the `Loadings` plot. Each point on the plot is a molecule with an `m/z` value and a `retention time` or `rt` shown as `m/z_rt`.

#### 2.4 Individual and Cumulative Variance
This plot shows how much variance each principal component (PC) is responsible for as well as the overall cumulative variance which sums to 100% for all of the PCs. As you select a node further down in the tree, PC1 and PC2 explain more and more of the total variance for that node.

## 3. Exit insert icon
Pressing the exit button will shut down the app.

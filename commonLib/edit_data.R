add_mass_column <-
    function(dataframe, rnames = rownames(dataframe)) {
        # Adds a column containing mz to the dataframe.
        dataframe = as.data.frame(dataframe)
        dataframe$m = str_replace(string = rnames,
                                  pattern = ".*_",
                                  replacement = "")
        dataframe$m = sapply(dataframe$m, as.numeric)
        return(dataframe)
    }
add_rt_column <-
    function(dataframe, rnames = rownames(dataframe)) {
        # Adds a column containing rt to the dataframe.
        dataframe = as.data.frame(dataframe)
        dataframe$rt = str_replace(string = rnames,
                                  pattern = "_.*",
                                  replacement = "")
        dataframe$rt = sapply(dataframe$rt, as.numeric)
        return(dataframe)
    }

add_euclidean_distance <-
  function(dataframe, x = dataframe[, 1], y = dataframe[, 2]) {
    # Calculates Euclidean distance between x and y and adds it to a
    # dataframe column named 'ed'. If no value is given, assumes 1st and 2nd
    # columns are 'x' and 'y' respectively. Returns the entire dataframe.
    dataframe = as.data.frame(dataframe)
    dataframe$ed = sqrt(x ^ 2 + y ^ 2)
    return(dataframe)
  }

scale_pareto = function(df) {
  #returns pareto scaled data
  df = as.data.frame(
    apply(
      X = df,
      MARGIN = 2,
      FUN = function(x) (x - mean(x)) / sqrt(sd(x))
      )
    )
}

fd.sort <-
  function(x) {
    # sort x from highest to lowest also returning the index
    sorted = sort(x = x,
                  decreasing = T,
                  index.return = T)
    return(sorted)
}

remove_zeros = function(df) {
  #remove columns with zero sum
  # sumcol = apply(df, 2, sum) #get the sum of all the columns
  sumcol = colSums(df)
  df = df[, sumcol != 0] # select only the non zero sum columns
}

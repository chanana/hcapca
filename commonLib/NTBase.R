# Edited on 2018-05-30 ----
# lookup_function_triplicate <-
#   function(x, b) {
#     alpha = rownames(b)[x[1]]
#     beta = rownames(b)[x[2]]
#     gamma = rownames(b)[x[3]]
#     if (alpha == beta & beta == gamma) {
#       result = x[1]
#     }
#     else {
#       result = -1
#     }
#     return(result)
#   }
# lookup_function_duplicate <-
#   function(x, b) {
#     gamma = rownames(b)[x[1]]
#     delta = rownames(b)[x[2]]
#     if (gamma == delta) {
#       result = x[1]
#     }
#     else {
#       result = -1
#     }
#     return(result)
#   }
different_function <-
  function(x, b) {
    # function modified on 2018-05-30 to assume that the dataframe b contains
    # only non-duplicate samples (rows)
    # if there are non-zero elements, this returns the indices of those elements in the list
    n = which(x != 0)
    # if there is only one non-zero element, return it.
    if (length(n) == 1) {
      result = n
    }
    # # if there are exactly two values, check if they are duplicates.
    # if (length(n) == 2) {
    #   result = lookup_function_duplicate(n, b)
    # }
    # # if there are exactly three values,check if they are triplicates.
    # if (length(n) == 3) {
    #   result = lookup_function_triplicate(n, b)
    # }
    # # if there are more than three values, return -1.
    # if (length(n) > 3) {
    #   result = -1
    # }
    else{
      result = -1
    }
    return(result)
  }

# end of edit ----

filter_M <-
  function(m, a, p) {
    which(abs((m - a) * 1000000 / a) <= p) #it is a hit
  }
filter_H <-
  function(m, a, p) {
    which(abs((m - PROTON - a) * 1000000 / a) <= p) #it is a hit
  }
filter_Na <-
  function(m, a, p) {
    which(abs((m - SODIUM_Plus - a) * 1000000 / a) <= p) #it is a hit
  }

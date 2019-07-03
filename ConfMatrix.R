# This function makes the confusion matrix and calculates the 
# cross-tabulation of two or more vectors consist of observed and 
# predicted classes. ConfMatrix also calculates the overall accuracy 
# as well as Recall and Precision in classes.

ConfMatrix <- function(pred, 
                       ref
) {
  
  # make the confusion matrix
  tab = table(Pred = pred, Ref = ref)
  
  Sum = 0
  for(i in 1:ncol(tab)) {
    for (j in 1:nrow(tab)) {
      if ( colnames(tab)[i] ==  rownames(tab)[j] ) {
        Sum = Sum + tab[i,j]    
      }
    }
  } 
  
  Accuracy = Sum / sum(tab)
  
  RecallTab = matrix(0, nrow = nrow(tab), 
                     ncol = ncol(tab), 
                     dimnames = list( Pred = rownames(tab), 
                                      Ref = colnames(tab) ))
  
  PrecisionTab = RecallTab
  colSums = rep(0, ncol(tab))
  for( i in 1:ncol(tab) ) colSums[i] =  sum(tab[,i])
  rawSums = rep(0, nrow(tab))
  for( i in 1:nrow(tab) ) rawSums[i] =  sum(tab[i,])
  
  for (i in 1:nrow(tab)){ # raw
    for(j in 1:ncol(tab)) { # column
      RecallTab[i,j] = as.numeric(format(tab[i,j]/colSums[j], digits = 2  ) )
      PrecisionTab[i,j] = as.numeric(format(tab[i,j]/rawSums[i], digits = 2  ) )
    }
  }
  
  # In class Precison and Recall
  RecallTab  
  PrecisionTab
  
  return( list(Accuracy = Accuracy,
               ConfMatrix = tab, 
               RecallMatrix = RecallTab, 
               PrecisionMatrix =PrecisionTab))
  
}

# End
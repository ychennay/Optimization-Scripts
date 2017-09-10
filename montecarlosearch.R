montecarlosearch= function(evaluationfunction, minormax){
  #check #: lower and upper bounds are same length
  stopifnot(length(lower) = length(upper))
    
  dimensions = length(lower)
  searchmatrix = matrix(nrow = numsamples, ncol = dimensions)
  
  for (i in 1:N){
    searchmatrix[i,] = runif(dimensions, lower, upper) # population a search matrix with a randum uniform distribution of numbers bounded by upper and lowers
    
    x=apply(searchmatrix,1,evaluationfunction) # applies the search function across each row of the matrix (each sample)
    index = switch(minormax, min=which.min(x), max=which.max(x)
    return(list(index=index,solution=searchmatrix[index,], evaluation=x[index]))
  }
}
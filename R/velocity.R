# velocity finding

# head moves velocity mean

moving_difference <- function(x){
  
  y <- array()
  for (i in 1:(length(x)-1)){
    y[i] = x[i+1] - x[i]
  }
  return(y)
}

moving_difference(c(1,4,-2, 2))

stats_velocity <- function(x, df = NULL){
  
  if(!is.null(df)){
    x = df[[x]]
  }
  
  y = statistics(abs(moving_difference(x)))
  names(y) = paste( "velocity", names(y), sep = "_")
  return(unlist(y))
}

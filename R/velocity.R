#' Calculate velocity.
#' calculate the difference of values in each ordered value of an array
#' @param x The array to transform
#' @return an array of same length (first value is NA)
#' @examples
#' moving_difference(c(1,4,-2, 2))



moving_difference <- function(x){

  y <- array()
  for (i in 1:(length(x)-1)){
    y[i] = x[i+1] - x[i]
  }
  return(c(NA, y))
}

### all statististics

statistics <- function(x){
  
  mx = mean(x, na.rm = T)
  medx = median(x, na.rm = T)
  sdx = sd(x, na.rm = T)
  minx = min(x, na.rm = T)
  maxx = max(x, na.rm = T)
  entx = entropy::entropy(x)
  
  descr = list(mx, medx, sdx, minx, maxx, entx)
  names(descr) = c("mean", "median", "sd", "min", "max", "entropy")
  
  return(descr)
  
}
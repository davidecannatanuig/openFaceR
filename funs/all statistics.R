### all statististics

statistics <- function(x, mean = T, median = T, sd = T, min = F, max = F, ent = F){
  
  descr = list()
    
  if (mean == T){
    descr[["mean"]] = mean(x, na.rm = T)
  }
  
  if(median == T){
    descr[["median"]] = median(x, na.rm = T)
  }
  
  if(sd == T){
    descr[["sd"]] = sd(x, na.rm = T)
  }
  
  if(min == T){
    descr[["min"]] = min(x, na.rm = T)
  }
  
  if(max == T){
    descr[["max"]] = max(x, na.rm = T)
  }
  
  if(ent == T){
    descr[["entropy"]] = entropy::entropy(x)
  }

  return(descr)
  
}

#TODO frequencies <- (x, type == "epm", sensitivity = 10)
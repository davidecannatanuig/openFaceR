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

#-----------------------------------------------------------------------------------------------


#' count events
#' Function to count howm many differnet time a facial event occurs
#' @param x the variable for which to count events (must have two levels: 0 and 1)
#' @param sensitivity how many frames might have the sodtware missed and still considered the event as one

count_events <- function(x, sensitivity = 5){
  
  count = x[[1]] # start a counter with the first condition (if the events it's present is counted])
  buffer = 0 # set a buffer to avoid measurement error 
             # (considering two events as separate when they are actually the same)
  
  for (i in 2:length(x)){
    if (x[i] > x[i - 1]){
      count = count + 1 # add an event to the counter if the event was not happening before and now happens
      buffer = 0 # reset buffer
    } else if(x[i] < x[i - 1] & buffer <= sensitivity){
      x[i] = 1
      buffer = buffer + 1
    }
  }
  
  return(count)
}

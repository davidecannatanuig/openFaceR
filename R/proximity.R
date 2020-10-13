#Proximity from camera function

# 1. Proximity

# This feature is calculated as the mean distance from the screen of the contour points of the face


calc_proximity <- function(df, plot = F){
  
  
  if(plot == T){
    plot(density(df$pose_Tz))
  }
  
  return(statistics(df$pose_Tz))
}

#example

#calc_proximity(my.df.list[[5]], plot = F)

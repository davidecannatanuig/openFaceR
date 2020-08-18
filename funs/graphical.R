############ Graphical functions

#### plot features in time variable ####
# allows to zoom in
# allows to chose how many features you want
# allows to standardise

feats_in_time <- function (df, vars, tmin = NULL, tmax = NULL, standardise = F, ...){
  
  ### ADD MORE PRESET OPTIONS ###
  
  if (vars == "head" | vars == "h"){
    vars = c("pose_Ty", "pose_Tx", "pose_Tz")
  }

  if(is.null(tmin)){
    tmin = min(df$timestamp, na.rm = TRUE)
  }
  
  if(is.null(tmax)){
    tmax = max(df$timestamp, na.rm = TRUE)
  }
  
  if(standardise == T){
    df = df %>%
      mutate(timestamp = as.character(timestamp)) %>%
      mutate_if(is.numeric, scale) %>%
      mutate(timestamp = as.numeric(timestamp))
  }
 
   df <- df %>%
    select_if(is.numeric) %>%
    gather(key = "feat", value = "value", - timestamp) %>%
    filter(feat %in% vars)
   
   percent3 = (max(df$value, na.rm = T) - min(df$value, na.rm = T)) * .03
   
   df %>%
    filter(timestamp >= tmin & timestamp <= tmax) %>%
    ggplot(aes(x = timestamp, y = value, color = feat)) +
    geom_line(...) +
    ylab(NULL) +
    ylim(min(df$value, na.rm = T) - percent3, max(df$value, na.rm = T + percent3))


  
  # add annotations
}


### example
#feats_in_time(my.df.list[[19]], "h", tmin = 17, tmax = 23, standardise = T)
#feats_in_time(my.df.list[[14]], "AU02_r")
#feats_in_time(my.df.list[[14]], "AU02_c")

#############################

#next is graph with events + lines

#my.df.list[[14]] %>%
#  ggplot(aes(x = timestamp, y = AU02_c*max(my.df.list[[14]]$AU02_r, na.rm = T))) +
#  geom_col(col = "dark grey") +
#  geom_line(aes(y = AU02_r), color = "dark green", size = 2)

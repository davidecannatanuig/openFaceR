### Set of fucntions to record statistics about peaks of movements. Especially used to identify large movements of the head


# create a function for moving average which add a few NA to match numbers
moving_av = function(x, n){
  y = zoo::rollmean(x, n)
  y2 = rep(NA, n-1)
  z = c(y, y2)
  return(z)
}


# function to find valleys (inverse of peak finding)

findvalleys <- function(x, ...){
  x = -x
  ret = pracma::findpeaks(x, ...)
  ret[,1] = -ret[,1]
  return(ret)
  
}
pracma::findpeaks(moving_av(my.df.list[[1]]$pose_Tx,10), threshold = 5)

findvalleys(moving_av(my.df.list[[1]]$pose_Tx,10), threshold = 5)


#pracma::findpeaks(-moving_av(my.df.list[[1]]$pose_Tx,10), threshold = 5)



# function to extract extract all the peaks of a nvb

nvb_peaks <- function(df, var, smooth = 10, thresh = 5){
  peaks = pracma::findpeaks(moving_av(df[[var]],smooth), 
                            # the function works on a moving average to avoid random fluctuations. Smooth refers to the smooth of the fluctuations
                            threshold = thresh)
                            #tresholds refers to how big the peaks need to be
  #smooth and thresholds have been manually optimised for head moves
  #TODO:  optimise through machine learning
  
  valleys = findvalleys(moving_av(df[[var]],smooth), threshold = thresh)
  
  # finds the valleys with same threshold and smooths
  
  peaks_vals = rbind(matrix(peaks, ncol = 4), matrix(valleys, ncol = 4)) %>%
    as.data.frame()
  
  names(peaks_vals) = c("intensity", "frame", "start", "end")
  
  peaks_vals <- peaks_vals %>%
    arrange(frame) %>%
    left_join(select(df, timestamp, frame)) %>%
    mutate(f_duration = end - start)
  
  return(peaks_vals)
}

#peaks_stats: a function to describe the distribution of peaks

peaks_stats <- function(df, var = NULL, smooth = 10, thresh = 5){
  
  if (!is.null(var)){
    peaks = nvb_peaks(df, var, smooth = smooth, thresh = thresh)
  } else{
    peaks = df # problem, in this case duration is underestimated
  }
  
  if ("intensity" %in% names(peaks) & "f_duration"  %in% names(peaks) ){
    
    stats_intensity <- statistics(peaks$intensity)
    names(stats_intensity) = paste("intensity", names(stats_intensity), sep ="_")
    stats_duration <-  statistics(peaks$f_duration)
    names(stats_duration) = paste("duration", names(stats_duration), sep = "_")
    frequency_peaks <- nrow(peaks)*60/max(df$timestamp, na.rm = T)
    stats <- unlist(c(stats_intensity, stats_duration, frequency_peaks))
    names(stats)[13] = "frequency_peaks"
    return(stats)
    
  } else {
    print("error: you need to provide a dataframe with peak characteristics
    and not specify any var or to provide an openface import df and specify an existing variable")
  }
  
}

# example

nvb_peaks(my.df.list[[20]], "pose_Tx")

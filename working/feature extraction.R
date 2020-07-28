# feature extraction

#relatred to stress: blink frequency DONE
# related to arousal: head movements DONE, eye widening
# related to friendliness: smiles, fake msiles (count DONE, maybe want to add mean)
# related to expressiveness: eyebrow up (AUC2, count done), wmei (TBD)
# related to fronatality: frequency of frontal events (TBD), number of tilts and shake (done), frequency of frontal gaze (TBD)





# 1. function to calculate all relevant statistics
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

# 1. Proximity

# This feature is calculated as the mean distance from the screen of the contour points of the face


calc_proximity <- function(df, plot = F){
  

  if(plot == T){
    plot(density(df$pose_Tz))
  }
  
  return(statistics(df$pose_Tz))
}

#example

calc_proximity(my.df.list[[5]], plot = F)

#do on many

prox.list2 = matrix(nrow = 20, ncol = 6)
for (i in 1:20) {
  prox.list2[i,1:6] = unlist(calc_proximity(my.df.list[[i]]))
}

colnames(prox.list) <- names(calc_proximity(my.df.list[[1]]))

prox.test = prox.list[1:10 * 2 -1,]
prox.retest = prox.list[1:10 * 2,]

cor(prox.test, prox.retest)


# 2. head motion (problem)


# 3. head_tilt_horizontal, vertical and z + rotation horizontal, vertical and z
# This feature is calculated in moving average (k = 10) and includes:
# 0. normal stats
# 1. events(number of times there is a change > 10))
# 2. mean velocity (the average of the absolute values of changes from a frame to the other)
# 3. mean acceleration (the average of the absolute values of changes of velocity from a frame to another)


# create a function for moving average which add a few NA to match numbers
moving_av = function(x, n){
  y = zoo::rollmean(x, n)
  y2 = rep(NA, n-1)
  z = c(y, y2)
  return(z)
}

### events statistics

# valley function

findvalleys <- function(x, ...){
  x = -x
  ret = pracma::findpeaks(x, ...)
  ret[,1] = -ret[,1]
  return(ret)
  
}
pracma::findpeaks(moving_av(my.df.list[[1]]$pose_Tx,10), threshold = 5)

findvalleys(moving_av(my.df.list[[1]]$pose_Tx,10), threshold = 5)

#nvb_peaks(my.df.list[[1]], "pose_Tx")

#pracma::findpeaks(-moving_av(my.df.list[[1]]$pose_Tx,10), threshold = 5)



# extract all the peaks

nvb_peaks <- function(df, var, smooth = 10, thresh = 5){
  peaks = pracma::findpeaks(moving_av(df[[var]],smooth), threshold = thresh)
  valleys = findvalleys(moving_av(df[[var]],smooth), threshold = thresh)
  
  peaks_vals = rbind(matrix(peaks, ncol = 4), matrix(valleys, ncol = 4)) %>%
    as.data.frame()
  
    names(peaks_vals) = c("intensity", "frame", "start", "end")
  
  peaks_vals <- peaks_vals %>%
    arrange(frame) %>%
    left_join(select(df, timestamp, frame)) %>%
    mutate(f_duration = end - start)
  
  return(peaks_vals)
}

#provide stats

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

# multiple

Tx.list = matrix(nrow = 20, ncol = 13)
for (i in 1:20) {
  Tx.list[i,1:13] = peaks_stats(my.df.list[[i]], "pose_Tx")
}
colnames(Tx.list) <- names(peaks_stats(my.df.list[[1]], "pose_Tx"))

Tx.test = Tx.list[1:10 * 2 -1,]
Tx.retest = Tx.list[1:10 * 2,]

cor(Tx.test, Tx.retest)

Tx.list


# multiple y

Ty.list = matrix(nrow = 20, ncol = 13)
for (i in 1:20) {
  Ty.list[i,1:13] = peaks_stats(my.df.list[[i]], "pose_Ty")
}
colnames(Ty.list) <- names(peaks_stats(my.df.list[[1]], "pose_Tx"))

Ty.test = Ty.list[1:10 * 2 -1,]
Ty.retest = Ty.list[1:10 * 2,]

cor(Ty.test, Ty.retest)

Tx.list



feats_in_time(my.df.list[[19]], "h", tmin = 0, tmax = 60, standardise = F)

??pracma::findpeaks
example_rot <- moving_av(pose_Tx, 10)

my.df.list[[19]] %>% filter(frame > 30 & frame < 40)

# t x is rotation of head (shaking), t y is rotation of head (nodding)  


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


# three dimensional velocity (measure of overall head movement)

velocity_3D <- function(df){
  x = abs(moving_difference(df[["pose_Tx"]]))
  y = abs(moving_difference(df[["pose_Ty"]]))
  z = abs(moving_difference(df[["pose_Tz"]]))
  xyz = x + y + z
  v3d = statistics(xyz)
  names(v3d) = paste( "head_movements", names(v3d), sep = "_")
  return(unlist(v3d))
}


velocity_3D(my.df.list[[10]])

# example
stats_velocity("pose_Ty", my.df.list[[3]])


stats_acceleration(my.df.list[[3]]$pose_Ty)

### check for all videos

velocity.Tx.list = matrix(nrow = 20, ncol = 6)
for (i in 1:20) {
  velocity.Tx.list[i,1:6] = unlist(stats_velocity(my.df.list[[i]]$pose_Tx))
}
colnames(velocity.Tx.list) <- names(stats_velocity(my.df.list[[1]]$pose_Tx))

v.test = velocity.Tx.list[1:10 * 2 -1,]
v.retest = velocity.Tx.list[1:10 * 2,]

cor(v.test, v.retest)


### Frequencies (number per minute)


# nmov indicates the moving average frame to avoid that an event is mistakenly considered as 2
count_events <- function(df, x, n_mov = 2){
  if (!is.null(df)) {
    x = df[[x]]
  }
  
  x = moving_av(x, n_mov)
  x = x[!is.na(x)]
  n = 0
  for (i in 1:length(x)){
    if (i == 1){
      state = x[1] > 0
    } else if(x[i] > 0 & state == F){
      n = n + 1
      state = x[i] > 0
    } else{
      state = x[i] > 0
    }
  }
return(n)
  
}
  

frequency <- function(df, x, n_mov = 5){
  60 * count_events(df, x, n_mov)/max(df$timestamp, na.rm = T)
}

# smiles, fake smiles, blink and eybrow frequency 

AU_C_engineering  <- function(df) {
  
  # engineers smile, blink, fake smile, and a measure of "amount of AUC events
  df <- df %>%
    mutate(smile = ifelse(AU06_c == 1 & AU12_c == 1, 1, 0)) %>%
    mutate(blink = AU45_c) %>%
    mutate(brow_up = AU02_c) %>%
    mutate(fake_smile = ifelse(AU06_c == 0 & AU12_c == 1, 1, 0))
}

ex1 <- AU_C_engineering(my.df.list[[10]])

frequency(ex1, "fake_smile")


feats_in_time(ex1, "brow_up")


## eyebrows

## wmei
               
# video analysis

library(tidyverse)

#

#import data
my.files <- list.files(path = "data/extracted_video_features/")
my.files <- paste("data/extracted_video_features/", my.files, sep = "")

my.df.list <- lapply(my.files, read_csv)

names(my.df.list[[1]])

example <- my.df.list[[1]] %>%
  select(frame:gaze_angle_y, pose_Tx, pose_Rz, AU01_r:AU45_c)

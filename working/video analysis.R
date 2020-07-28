# video analysis

library(tidyverse)

#

#import data
my.files <- list.files(path = "video/")
my.files <- paste("extracted_video_features/", my.files, sep = "")

my.df.list <- lapply(my.files, read_csv)

names(my.df.list[[1]])

example <- my.df.list[[1]] %>%
  select(frame:gaze_angle_y, pose_Tx, pose_Rz, AU01_r:AU45_c)

mean(example$confidence)
sd(example$gaze_0_x)
sd(example$gaze_0_y)
sd(example$gaze_0_z)

mean(example$gaze_0_x)
mean(example$gaze_0_y)
mean(example$gaze_0_z)

### 


example <- example%>% 
  mutate(smile = ifelse(AU06_c == 1 & AU12_c == 1, TRUE, FALSE)) %>%
  mutate(blink = ifelse(AU45_c == 1, TRUE, FALSE)) %>%
  mutate(fear = sqrt(AU01_r) + sqrt(AU02_r) + sqrt(AU04_r) + sqrt(AU05_r) + sqrt(AU07_r) + sqrt(AU20_r) + sqrt(AU26_r)) %>%
  mutate(fake_smile = ifelse(AU06_c == 0 & AU12_c == 1, TRUE, FALSE)) %>%
  mutate(wmei = AU01_r + AU02_r + AU04_r + AU05_r + AU06_r + AU07_r + AU09_r + AU10_r + AU12_r + AU14_r + AU15_r + AU17_r + 
           AU20_r + AU23_r + AU25_r + AU26_r + AU45_r) %>%
  mutate(wmeic = AU01_c + AU02_c + AU04_c + AU05_c + AU06_c + AU07_c + AU09_c + AU10_c + AU12_c + AU14_c + AU15_c + AU17_c + 
           AU20_c + AU23_c + AU25_c + AU26_c + AU45_c) 
mean(example$smile)
mean(example$blink)
mean(example$fear)
mean(example$fake_smile)
mean(example$wmei)
mean(example$wmeic)

which.max(my.df.list[[1]][["pose_Tx"]])
my.df.list[[1]][[737, 5]]



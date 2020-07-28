# video analysis

library(tidyverse)

my.functions <- list.files(path = "funs/")
plyr::a_ply(.data = my.functions, .margins = 1, .fun = function(x) source(paste("funs/", x, sep = ""))) 

### transform the videos in csv inputting the result string in the command line. 
### The operation might take a considerable amount of time

get_commands("C/programs/openface/openface.exe", "myvideos/videoex", "myoutput")

# open csv files
my.df.list <- read_of_csv("data/extracted_video_features/")

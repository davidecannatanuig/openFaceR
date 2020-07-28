## import all the extracted file into a list in which each element correspond to a video

read_of_csv <- function(outputfolder){
  
  my.files <- list.files(path = outputfolder)
  my.files <- paste(outputfolder, my.files, sep = "")
  my.df.list <- lapply(my.files, read_csv)
  return(my.df.list)
}

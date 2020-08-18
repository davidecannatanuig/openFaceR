## import all the extracted file into a list in which each element correspond to a video

read_face_csvs <- function(outputfolder){
  
  my.files <- list.files(path = outputfolder)
  my.paths <- paste(outputfolder, my.files, sep = "")
  my.names <- str_remove(my.files, ".csv")
  my.df.list <- lapply(my.paths, read_csv)
  names(my.df.list) = my.names
  return(my.df.list)
}

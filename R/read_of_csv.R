## import all the extracted file into a list in which each element correspond to a video

#' read face csvs
#' Function to read all the csvs from one analysis into one face object
#' @param: output_dir the folder where csvs outputs are stored
#' @output a face object

read_face_csvs <- function(output_dir){

  my.files <- list.files(path = output_dir)
  my.paths <- paste(output_dir, my.files, sep = "")
  my.names <- stringr::str_remove(my.files, ".csv")
  my.df.list <- lapply(my.paths, read_csv)
  names(my.df.list) = my.names
  #TODO: make it a face object
  return(my.df.list)
}

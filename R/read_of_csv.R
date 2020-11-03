#' Read OpenFace output.
#' @description Function to read all the csvs from one analysis into one face object.
#' @param: output_dir the folder where csvs outputs are stored.
#' @return A face object.
#'
#' @examples
#' #create a directory with Lara and Teddy
#' write_csv(lara, "newfolderex//lara.csv)
#' write_csv(teddy, "newfolderex//teddy.csv)
#' # Import Lara and Teddy data
#' read_csv("newfolderex")
#' @export

read_face_csvs <- function(output_dir){

  my.files <- list.files(path = output_dir)
  my.paths <- paste(output_dir, my.files, sep = "")
  my.names <- stringr::str_remove(my.files, ".csv")
  my.df.list <- lapply(my.paths, readr::read_csv)
  names(my.df.list) = my.names
  #TODO: make it a face object
  return(my.df.list)
}

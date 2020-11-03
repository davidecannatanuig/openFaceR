#' Dealing with large collections of csvs
#' @desc sometimes research might find themselves dealing with a large amount of files. R does not deal well
#' with very large lists as it runs out of memory when trying to transform too many data all together.
#' Openfacer provides the option of working with models, strings object that include the full openfacer pipe.
#' Models can be used with two methods. The first, faster, read and transform each csv file separately, so that each
#' large dataframe is transormed immediately in a single row, reducing a lot the strain on the memory. It is applied
#' through the *read_and_tidy()* function.
#'
#' The second method, allows the usser to save the list of files as large lists object (from the LargeList package)
#' through the function *ll_read_of_csv*. The method is preferred if the researcher wants to save a copy
#' of the faces list. The list can then be unpacked in small batches over which the mode lis applied using
#' *ll_tidyface()*.
#'
#' Both the method work well on regular laptop on batches of 500 csv files.
#'
#' @param output_dir There location where csvs files are stored.
#' @param model A string with the unparsed openfacer model.
#' @param file the name of the large list file (.llo) that will be created in your directory. Only for *ll_read_of_csv*.
#' @param df. The list file on which *ll_tidyface* works
#' @param c the size batches for *ll_tidyface*. A larger number will make the process faster but increase the
#' probabilities of memory failure. Default is 10.
#'
#' @examples
#' # Create a model
#'
#' my.model <-  "filter_faces(success == 1) %>%
#' select_faces(AU02_r:AU45_r) %>%
#'  tidy_face(sd = FALSE)"
#'
#'  # With read_and_tidy
#'
#'  myface_summary <- read_and_tidy("my_file_dir/", model = my.model)
#'
#'  # With large lists
#'
#'  my_face_list <- ll_read_of_csv("my_file_dir/", file = "my_face_list.llo")
#'  myface_summary <- ll_tidyface(df = my_face_list, model = my.model, c = 10)
#'
#' @name large_lists

NULL

#' @rdname large_lists
#' @export


read_and_tidy <- function (output_dir, model) {

  my.files <- list.files(path = output_dir)
  my.paths <- paste(output_dir, my.files, sep = "")
  my.names <- stringr::str_remove(my.files, ".csv")

  model_parsed <- parse(text = paste("tidyf <- m %>%", model, sep = " "))

  for (i in 1:length(my.paths)){

    mlist <- list()
    m <- list(readr::read_csv(my.paths[[i]]))
    names(m) <- my.names[[i]]
    eval(model_parsed)
    mlist[[i]] <- tidyf
    print(my.names[i])
  }

  return(reduce(mlist, rbind))

}

#' @rdname large_lists
#' @export


ll_read_of_csvs <- function (output_dir, file = "my_face_list.llo") {

  my.files <- list.files(path = output_dir)
  my.df.list = getList(file, verbose = FALSE, truncate = TRUE)
  my.paths <- paste(output_dir, my.files, sep = "")
  my.names <- stringr::str_remove(my.files, ".csv")
  saveList(object = list(
    readr::read_csv(file = my.paths[[1]])),
    file = file, append = FALSE)

  for (i in 2:length(my.paths)){

    print(i)
    my.df.list[] <- list(readr::read_csv(my.paths[[i]]))
  }

  names(my.df.list) = my.names
  return(my.df.list)
}



#' @rdname large_lists
#' @export

ll_tidyface <- function(df, model, size = 10) {

  a <- length(df) %/% size
  b <- length(df) %% size

  mlist <- list()
  if(a > 0) {
    for (i in 1:a){
      m <- df[(size * (i - 1) + 1):(size * i)]
      eval(parse(text = paste("tidyf <- m %>%", model, sep = " ")))
      mlist[[i]] <- tidyf
    }
  }

  if(b != 0) {
    m <- df[(size * a + 1):(size * a + b)]
    eval(parse(text = paste("tidyf <- m %>%", model, sep = " ")))
    mlist[[a+1]] <- tidyf
  }
  return(reduce(mlist, rbind))

}

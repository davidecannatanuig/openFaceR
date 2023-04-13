# The four metafunction of OpenFaceR:
# mutate_faces, select_faces, filter_faces and tidy_faces


#' Selecting variables in faces objects.
#' @description Select a subset of the variables across a faces object.
#' It always return timestamp, frame and success.
#' The function is an openfacer equivalent of dplyr::select.
#' @param faces A "faces" object.
#' @param ... The names or the numbers of the selected variables, or an expression that point to them.
#' The operator "-" exclude the variables.
#' @return A "faces" object with the selected variables, timestamp, frame and success.
#' @examples
#'
#' # Select the AU related to smiling
#' select_faces(test_faces, AU12_c, AU06_c)
#'
#' # Create a mei variable, select it and summarise
#' test_faces %>%
#' transform_faces("mei", mei) %>%
#' select_faces(mei) %>%
#' tidy_face()
#'
#' # Select all the AU variables
#' test_faces %>%
#' select_faces(starts_with("AU"))
#'
#' @export

select_faces <- function(faces, ...) {

  lapply(faces, function(x) dplyr::select(x, timestamp, frame, success, ...))
  #timestamp, frame and success are alywas kept

}



#' Filtering faces objects.
#' @description Filters across all the dataframes in a faces object.
#' @param faces The face object to filter.
#' @param ... The filtering condition. The function will delete all the rows in all the dataframes
#' for which the condition is false.
#'
#' @return The function returns a filtered "faces" object
#' @examples
#'
#'  # Delete unsuccessful trials
#'
#'  filter_faces(test_faces, success == 1)
#'
#'  # Take only the first minute of each video analysed
#'
#'  test_faces %>%
#'  filter_faces(timestamp < 60)
#' @export

filter_faces <- function(faces, ...) {

  lapply(faces, function(x) dplyr::filter(x, ...))

}

#' Mutating and transforming faces objects.
#' @description Mutate_faces and transfrom_faces are metafunctions that allow to create new variables in all the dataframes of a face object.
#' Mutate_faces adds a new variables by writing their name and a formula (see examples).
#' It is the openfacer equivalent of dplyr::mutate.
#' Transform_faces use functions that take as input a whole face dataframe.
#' @inheritParams select_faces
#' @inheritParams dplyr::mutate
#' @param var The name, as a string, of the new variable created (transform_faces).
#' @param fun The name of the function to use (transform_faces)
#' @param ... only in mutate_faces. Pairs of expression inluding a name and the formula for caluclating the value.
#'
#' @examples
#'
#' #Create a "smiles" variable
#' #' mutate_faces(test_faces, smile = ifelse(AU06_c == 1 & AU12_c == 1, 1, 0))
#'
#' #Create a mei variable, select it and summarise
#' test_faces %>%
#' transform_faces("mei", mei) %>%
#' select_faces(mei) %>%
#' tidy_face()
#'
#' @name transformations

NULL

#' @rdname transformations
#' @export

mutate_faces <- function(faces, ...) {

  lapply(faces, function(x) dplyr::mutate(x, ...))

}

#' @rdname transformations
#' @export

transform_faces <- function(faces, var, fun){

  for (i in seq_along(faces)) {

    faces[[i]][[var]] = fun(faces[[i]])
  }

  return(faces)
}






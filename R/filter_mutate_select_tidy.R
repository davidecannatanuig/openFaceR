# The four metafunction of OpenFaceR:
# mutate_faces, select_faces, filter_faces and tidy_faces


#' Filter

filter_faces <- function(faces, ...) {

  lapply(faces, function(x) dplyr::filter(x, ...))

}

#' Mutate

mutate_faces <- function(faces, ...) {

  lapply(faces, function(x) dplyr::mutate(x, ...))

}


#' Select

select_faces <- function(faces, ...) {

  lapply(faces, function(x) dplyr::select(x, timestamp, frame, success, ...))
  #timestamp, frame and success are alywas kept

}


#'Transform
#'unlike mutate it takes function taking as argument the full dataset rather than dataset variables

transform_faces <- function(faces, var, fun){

  for (i in seq_len(faces)) {

    faces[[i]][[var]] = fun(faces[[i]])
  }

  return(faces)
}


#TODO exceptions handling
#TODO pipe that allows to chose variables from list



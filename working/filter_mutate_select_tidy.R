# The four metafunction of OpenFaceR:
# mutate_faces, select_faces, filter_faces and tidy_faces


#' Filter

filter_faces <- function(faces, ...){
  
  lapply(faces, function(x) filter(x, ...))
  
}

#' Mutate

mutate_faces <- function(faces, ...){
  
  lapply(faces, function(x) mutate(x, ...))
  
}


#' Select

select_faces <- function(faces, ...){
  
  lapply(faces, function(x) select(x, ...))
  
}


#TODO exceptions handling
#TODO pipe that allows to chose variables from list


a <- my.df.list %>%
  select_faces (frame, timestamp)



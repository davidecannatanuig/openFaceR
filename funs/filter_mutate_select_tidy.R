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


#' transform
trasform_faces <- function(faces, var, fun){
  
  #unlike mutate it takes function taking as argument the full dataset rather than dataset variables
  
  for (i in 1:length(faces)){
    
    faces[[i]]$var = fun(faces[[i]])
  }
  
  return(faces)
}

#TODO exceptions handling
#TODO pipe that allows to chose variables from list



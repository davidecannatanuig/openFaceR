#' Motion Energy Intensity
#' @details The function calculate the average energy of motion of 67 the facial points.The frame to frame difference in movement of each facial point is calculated after correcting for the overall poistion of the face. The change of each point is summed for each frame and the resulting vector is averaged.
#' @param df: either a dataframe coming from openFace output (for single faces) or an object of class faces (for multiple faces).  
#' @outpout: a vector with one motion energy per face
#' @author Davide Cannata - NUIG



# first, a support function for caculating motion energy for each coordinate

coord_mei <- function(df, shift){ # 
  sapply(df, function(x) moving_difference(x - shift)) %>% #calculate difference between position in each frame
    abs() %>% # calculate absolute value of the movement
    apply(MARGIN = 1, FUN = sum) %>% # sum by row
    mean() #mean of the resulting values
}


# CASE A: a single face
mei_single <- function(df){ # the case with dataframe
  
  
  x_shift <- df$pose_Tx # the x y and z correction for movement of the head
  y_shift <- test$pose_Ty  
  z_shift <- test$pose_Tz
  
  ##TODO: should also correct for pitch, yaw and roll?
  
  xmei <- test %>% select(X_0:X_67) %>% coord_mei(x_shift)
  ymei <- test %>% select(Y_0:Y_67) %>% coord_mei(y_shift)
  zmei <- test %>% select(Z_0:Z_67) %>% coord_mei(z_shift)
  
  mei <- sum(xmei, ymei, zmei)
  
  return(mei)
  
}

# CASE B: multiple cases

mei_faces <- function(df){ # applies the mei functio to a faces object or  list
  sapply(df, mei_single)
}

# WRAPPER: recognise when case A and when case B

mei <- function(df){
  if ("list" %in% (class)(df)) { #TODO change to "faces" when cretaing the faces class
    mei_faces(df)
  } else {
    mei_single(df)
  }
}



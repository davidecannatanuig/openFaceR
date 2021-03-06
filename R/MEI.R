#' Motion Energy Intensity
#' @details The function calculate the average energy of motion of 67 the facial points.The frame to frame difference in movement of each facial point is calculated after correcting for the average position of the most rigid points (nose bridge and face sides). The change of each point is summed for each frame and the resulting vector is averaged.
#' @param df: Either a dataframe coming from openFace output (for single faces) or an object of class "faces" (for multiple faces).
#' @return A vector with one motion energy per face.
#' @examples
#'
#' # Calculate Lara frame to frame mei
#' mei(lara)
#'
#' # Calculate average mei for the four example videos
#' test_videos %>%
#'   transform_videos("mei", mei) %>%
#'   select_videos(mei) %>%
#'   tidy_face()
#'
#' @note This is still a draft and the final version will use a different algorithm
#' @name MEI

NULL

# CASE A: a single face
#' @rdname MEI
mei_single <- function(df) {

  # first shifts of position of the whole face are calcultaed by using the less independently "moveable" facial points
  x_shift <- average_mov(df %>% select(X_0:X_4, X_12:X_16, X_27:X_30))
  y_shift <- average_mov(df %>% select(Y_0:Y_4, Y_12:Y_16, Y_27:Y_30))
  z_shift <- average_mov(df %>% select(Z_0:Z_4, Z_12:Z_16, Z_27:Z_30))

  ##TODO: should also correct for pitch, yaw and roll?

  xmei <- df %>% select(X_0:X_67) %>% coord_mei(x_shift)
  ymei <- df %>% select(Y_0:Y_67) %>% coord_mei(y_shift)
  zmei <- df %>% select(Z_0:Z_67) %>% coord_mei(z_shift)

  mei <- xmei + ymei + zmei

  mei <- c(NA, mei)

  return(mei)

}

# CASE B: multiple cases
#' @rdname MEI

mei_faces <- function(df, ...) {
  # applies the mei functio to a faces object or  list
  meis <- t(sapply(df, function(x) statistics(x = mei_single(x, ...)))) %>%
    as.data.frame()
  names(meis) <- paste(names(meis), "mei", sep = "_")
  return(meis)
}

# WRAPPER: recognise when case A and when case B
#' @rdname MEI

mei <- function(df, ...) {
  if ("list" %in% (class)(df)) {
    #TODO change to "faces" when cretaing the faces class
    mei_faces(df, ...)
  } else {
    mei_single(df)
  }
}

#--------------------------------------------------------------------------


#' Average Point Movements
#' @description Support function for MEI for calculating average movement of all the contour points.
#' @inheritParams mei

average_mov <- function(df) {
  shift <- apply(df, 1, function(x) mean(x, na.rm = T)) %>%
    moving_difference()

  return(shift)
}



#' Mei per coordinate
#' @description  Support function for caculating motion energy for each coordinate
#' @inheritParams mei


coord_mei <- function(df, shift) {
  sapply(df, function(x) moving_difference(x) - shift) %>%
    #calculate difference between position in each frame
      abs() %>% # calculate absolute value of the movement
      apply(MARGIN = 1, FUN = sum) # sum by row
       #mean of the resulting values
}


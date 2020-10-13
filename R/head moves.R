#' Head Movements.
#' @description Calculates the overall amount of head movements in the three axis.
#' @param df A dataframe with facial information from OpenFace.
#' @return A frame by frame array with the total difference of spatial position.
#'
#' @examples
#'
#' # Calculate Lara frame to frame change in head position
#' velocity_3d(lara)
#'
#' # Calculate average velocity of head for the four example videos
#' test_videos %>%
#'   transform_videos("velocity", velocity_3d) %>%
#'   select_videos(velocity_3d) %>%
#'   tidy_face()
#'
#'@export

velocity_3d <- function(df) {
  x <- abs(moving_difference(df[["pose_Tx"]]))
  y <- abs(moving_difference(df[["pose_Ty"]]))
  z <- abs(moving_difference(df[["pose_Tz"]]))
  xyz <- x + y + z
  return(xyz)
}

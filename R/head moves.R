#' head movements
#' three dimensional velocity (measure of overall head movement)

velocity_3d <- function(df) {
  x <- abs(moving_difference(df[["pose_Tx"]]))
  y <- abs(moving_difference(df[["pose_Ty"]]))
  z <- abs(moving_difference(df[["pose_Tz"]]))
  xyz <- x + y + z
  v3d <- statistics(xyz)
  names(v3d) <- paste( "head_movements", names(v3d), sep = "_")
  return(unlist(v3d))
}

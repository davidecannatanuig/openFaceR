#' draw_coordinates
#' function with two options to draw a face or eyes at a sepcific frame or time


# draw face to understand coordinate
draw_coordinates <- function(df, f = NULL, t = NULL, region = "face") {
  if (is.null(f)) {
    a <- which(abs(df$timestamp - t) == min(abs(df$timestamp - t)))
    f <- df$frame[a]
  }

  if (region == "face") {
  draw_coords <- df %>%
    dplyr::select(frame, X_0:Z_67) %>%
    dplyr::filter(frame %in% c(f)) %>%
    tidyr::gather(key = "coordinate", value = "value", - frame) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(coord = stringr::str_split(coordinate, pattern = "_", simplify = T)[[1]],
          number = stringr::str_split(coordinate, pattern = "_")[[1]][[2]]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::select(frame, coord, number, value) %>%
    tidyr::spread(key = coord, value = value)
  } else if (region == "eyes") {
    draw_coords <- df %>%
      dplyr::select(frame, eye_lmk_x_0:eye_lmk_Z_55) %>%
      dplyr::filter(frame %in% c(1)) %>%
      tidyr::gather(key = "coordinate", value = "value", - frame) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(coord = stringr::str_split(coordinate, pattern = "_", simplify = T)[[3]],
             number = stringr::str_split(coordinate, pattern = "_")[[1]][[4]]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(number = as.numeric(number)) %>%
      dplyr::select(frame, coord, number, value) %>%
      tidyr::pread(key = coord, value = value)
  }

  face_plot <- draw_coords %>%
    ggplot2::ggplot(aes(x = X, y = -Y, alpha = -Z)) +
    ggplot2::geom_point() +
    ggplot2::coord_fixed()

  face <- list(draw_coords, face_plot) # returns the coordinates and the plot

  return(face)
}


# TODO: interactive version selective frame
# TODO: 3D version

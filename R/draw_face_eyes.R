#' Draw coordinates.
#' @description Function with two options to draw a face or eyes at a sepcific frame or time.
#' @param df A face object or a dataframe with face values
#' @param f The chosen video frame for visualising the coordinates. This value will be ignored if you specify the time.
#' @param t The time in second at which you want to see the coordinates.
#' @param region either the whole face or the eyes.
#' @return a list including a matrix with the x, y and z coordinate of each point and a ggplot object with a graphical representation
#' @examples
#'
#' # Draw Paula face at frame 100
#' draw_coordinates(paula, f = 100)
#'
#' # Draw Lara eyes at second 12
#'  draw_coordinates(lara, t = 12, region = "eyes")
#' @export

# draw face to understand coordinate
draw_coordinates <- function(df, f = NULL, t = NULL, region = "face") {
  if (is.null(t) & is.null(f)){
    stop('you need to specify the frame or the time')
  }

  if(is.list(df)) { #TODO change for is.face

    lapply(df, function(x) draw_coordinates(x, f = f, t = t, region = region))
  } else {

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

}



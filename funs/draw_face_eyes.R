# function with two options to draw a face or eyes at a sepcific frame or time

require(tidyverse)

# draw face to understand coordinate
draw_coordinates <- function(df, f = NULL, t = NULL, region = "face"){
  if(is.null(f)){
    a <- which(abs(df$timestamp - t) == min(abs(df$timestamp - t)))
    f = df$frame[a]
  }
  
  if(region == "face"){
  draw_coords <- df %>%
   select(frame, X_0:Z_67) %>%
   filter(frame %in% c(f)) %>%
   gather(key = "coordinate", value = "value", - frame) %>%
   rowwise() %>%
   mutate(coord = str_split(coordinate, pattern = "_", simplify = T)[[1]],
          number = str_split(coordinate, pattern = "_")[[1]][[2]]) %>%
    ungroup() %>%
    mutate(number = as.numeric(number)) %>%
    select(frame, coord, number, value) %>%
    spread(key = coord, value = value)
  } else if (region == "eyes"){
    draw_coords <- df %>%
      select(frame, eye_lmk_x_0:eye_lmk_Z_55) %>%
      filter(frame %in% c(1)) %>%
      gather(key = "coordinate", value = "value", - frame) %>%
      rowwise() %>%
      mutate(coord = str_split(coordinate, pattern = "_", simplify = T)[[3]],
             number = str_split(coordinate, pattern = "_")[[1]][[4]]) %>%
      ungroup() %>%
      mutate(number = as.numeric(number)) %>%
      select(frame, coord, number, value) %>%
      spread(key = coord, value = value)
  } else if (region == "eyes"){
    draw_coords <- df %>%
      select(frame, eye_lmk_x_0:eye_lmk_Z_55) %>%
      filter(frame %in% c(1)) %>%
      gather(key = "coordinate", value = "value", - frame) %>%
      rowwise() %>%
      mutate(coord = str_split(coordinate, pattern = "_", simplify = T)[[3]],
             number = str_split(coordinate, pattern = "_")[[1]][[4]]) %>%
      ungroup() %>%
      mutate(number = as.numeric(number)) %>%
      select(frame, coord, number, value) %>%
      spread(key = coord, value = value)
    }
  

  face_plot <- draw_coords %>%
    ggplot(aes(x = X, y = -Y, alpha = -Z)) +
    geom_point()+
    coord_fixed()
  
  face <- list(draw_coords, face_plot) # returns the coordinates and the plot
  
}

#example 
draw_coordinates(example, t = 2, region = "face")[2]


# TODO: interactive version selective frame
# TODO: 3D version
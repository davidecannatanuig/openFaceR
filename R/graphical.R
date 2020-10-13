############ Graphical functions

#' features in time
#' visualise the level of a feature in one face, across time.
#' @param df the dataframe to visualise
#' @param vars the variable(s) to visualise
#'

feats_in_time <- function (df, vars, tmin = NULL, tmax = NULL, standardise = F, ...) {

  ###TODO ADD MORE PRESET OPTIONS ###
  ###TODO Add the possibility of comparing multiple faces ###

  if (vars == "head" | vars == "h") { #preset to look at the head
    vars <- c("pose_Ty", "pose_Tx", "pose_Tz")
  }

  if(is.null(tmin)) { #starting time
    tmin <- min(df$timestamp, na.rm = TRUE)
  }

  if(is.null(tmax)) { #end time
    tmax <- max(df$timestamp, na.rm = TRUE)
  }

  if(standardise == T) { # standardsation of vars
    df <- df %>%
      dplyr::mutate(timestamp = as.character(timestamp)) %>%
      dplyr::mutate_if(is.numeric, scale) %>%
      dplyr::mutate(timestamp = as.numeric(timestamp))
  }

   df <- df %>% # create dataframe for visualisation
     dplyr::select_if(is.numeric) %>%
    tidyr::gather(key = "feat", value = "value", - timestamp) %>%
     dplyr::filter(feat %in% vars)

   percent3 = (max(df$value, na.rm = T) - min(df$value, na.rm = T)) * .03 #ylim

   df %>%
     dplyr::filter(timestamp >= tmin & timestamp <= tmax) %>%
    ggplot2::ggplot(aes(x = timestamp, y = value, color = feat)) +
    ggplot2::geom_line(...) +
    ggplot2::ylab(NULL) +
    ggplot2::ylim(min(df$value, na.rm = T) - percent3, max(df$value, na.rm = T + percent3))

}

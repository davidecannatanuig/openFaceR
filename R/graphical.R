############ Graphical functions

#' features in time
#' visualise the level of a feature in one face, across time.
#' @param df The dataframe to visualise.
#' @param vars The variable(s) to visualise. There is a list of preset functions:
#' "head" or "h" allows to visualise the movements of the head in the three spatial axis.
#' @param tmin Starting time
#' @param tmax Ending time
#' @param standardise
#'  If true, standardises all the variables (to use when comparing variables with different levels)
#' @return A plot with the level of one or more features in time
#'
#' @examples
#'
#' # see the two AU relative to smiles for John, from second 10 to second 30
#' feats_in_time(john, c("AU_06r", "AU_12r"), tmin = 10, tmax = 30, standardise = F)
#'
#' # Paula standardised head movements
#' feats_in_time(paula, "h", standardise = TRUE)
#' @export

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

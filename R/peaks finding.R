#' Peaks.
#' @description Set of functions to record statistics about peaks of movements. Especially used to identify distinct movements of the head.
#' @param df the dataframe in which the variables are stored or a dataframe of peaks values.
#' @param var the target variable in the dataframe.
#' @param smooth the smooth parameter is used to smooth the descriptive curve and avoid the inclusion of very small differences in values. The default (advised) is 10.
#' @param tresh it indicates the minimum threshold to use to determine peaks used by Pracma "findPeaks" function. The  default (5) is advised.
#' @examples
#' # Describes the frequency of Teddy moving the head horizontaly
#' peaks_stats(teddy, var = pose_Tx, smooth = 10, thresh = 5)
#'
#' # Find the peaks of horizontal head motion for Lara
#' nvb_peaks(lara, var = pose_Tx, smooth = 10, thresh = 5)
#'
#' # Find Lara peaks and sumarise them (with conservative parameters)
#' lara_peaks <- nvb_peaks(lara, var = pose_Tx, smooth = 20, thresh = 10)
#' peaks_stats(lara_peaks)
#' @name peaks
NULL


#' @rdname peaks
#' @export

peaks_stats <- function(df, var = NULL, smooth = 10, thresh = 5){

  if (!is.null(var)){
    peaks = nvb_peaks(df, var, smooth = smooth, thresh = thresh)
  } else{
    peaks = df # problem, in this case duration is underestimated
  }

  if ("intensity" %in% names(peaks) & "f_duration"  %in% names(peaks) ){

    stats_intensity <- statistics(peaks$intensity)
    names(stats_intensity) = paste("intensity", names(stats_intensity), sep ="_")
    stats_duration <-  statistics(peaks$f_duration)
    names(stats_duration) = paste("duration", names(stats_duration), sep = "_")
    frequency_peaks <- nrow(peaks)*60/max(df$timestamp, na.rm = T)
    stats <- unlist(c(stats_intensity, stats_duration, frequency_peaks))
    names(stats)[13] = "frequency_peaks"
    return(stats)

  } else {
    print("error: you need to provide a dataframe with peak characteristics
    and not specify any var or to provide an openface import df and specify an existing variable")
  }

}


#' @rdname peaks
#' @export

nvb_peaks <- function(df, var, smooth = 10, thresh = 5){
  peaks = pracma::findpeaks(moving_av(df[[var]],smooth),
                            # the function works on a moving average to avoid random fluctuations. Smooth refers to the smooth of the fluctuations
                            threshold = thresh)
  #tresholds refers to how big the peaks need to be
  #smooth and thresholds have been manually optimised for head moves
  #TODO:  optimise through machine learning

  valleys = findvalleys(moving_av(df[[var]],smooth), threshold = thresh)

  # finds the valleys with same threshold and smooths

  peaks_vals = rbind(matrix(peaks, ncol = 4), matrix(valleys, ncol = 4)) %>%
    as.data.frame()

  names(peaks_vals) = c("intensity", "frame", "start", "end")

  peaks_vals <- peaks_vals %>%
    dplyr::arrange(frame) %>%
    dplyr::left_join(select(df, timestamp, frame)) %>%
    dplyr::mutate(f_duration = end - start)

  return(peaks_vals)
}

#' Moving average.
#' @description The function calculate the moving average of an array of numbers and add NAs in the end to have the same number of arguments.
#' @inheritParams peaks
#' @param x a numerical array.

moving_av = function(x, smooth){
  y = zoo::rollmean(x, smooth)
  y2 = rep(NA, n-1)
  z = c(y, y2)
  return(z)
}


#' Find valleys.
#'
#' @description This is the inverse of peak finding, and finds negative peaks.
#' @inheritParams moving_av

findvalleys <- function(x, ...){
  x = -x
  ret = pracma::findpeaks(x, ...)
  ret[,1] = -ret[,1]
  return(ret)

}






#' Count events.
#' @description Function to count how many differnet times a facial event occurs.
#' @param x the variable for which to count events (must have two levels: 0 and 1).
#' @param sensitivity how many frames might have the software missed and still considered the event as one.
#' @return The number of occurred events.
#' @examples
#' #Counting John blinkings
#' count_events(john$AU45_c)
#'
#' # Using a lower sensitivity to make sure each each blink is counted as one
#' count_events(john$AU45_c, sensitivity = 0)
#' # There will be higher risk of machine error
#'
#' @export

count_events <- function(x, sensitivity = 5) {
  if (!(levels(as.factor(x)) %in% c(0,1))) {
    stop("You need to use a dychotomic variable with two levels: 0 and 1!")
  }
  count <- x[[1]] # start a counter from 1o or 0 (if the events it's present)
  buffer <- 0 # set a buffer to avoid measurement error
             # (same event counted as separate for one miss)

  for (i in 2:length(x)) {
    if (x[i] > x[i - 1]) {
      count <- count + 1
      # add an event to the counter if new instance appears
      buffer <- 0 # reset buffer
    } else if (x[i] < x[i - 1] & buffer <= sensitivity){
      x[i] <- 1
      buffer <- buffer + 1
    }
  }

  return(count)
}

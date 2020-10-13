#Example:

#read_faces(dir) %>%
#mutate_face(velocity = velocity(), mei = mei()) %>%
#select_face(AU_12, gaze_y, velocity, mei) %>%
#tidy_face(mean = T, sd = T, median = F)


#' Tidy Face
#' A core function of the OpenFaceR routine. It summarise all the columns of a dataframe into a tidy dataset
#' @param x a faces object
#' @param events include events variables summary
#' @param continuous include countinous variables summary
#' @param events_sum the method for summarising events. This can be "eps" (events per second), "count" or "rate", which is the proportion of time in which events are happening
#' @param ... more options to control the summary of continuous variables (see continuous_summarise)
#' @return A list of "faces" dataframes
#' @examples
#'
#' # Calculate median mei for the four example videos
#' test_videos %>%
#'   transform_videos("mei", mei) %>%
#'   select_videos(mei) %>%
#'   tidy_face(mean = F, sd = T, median = F)
#'
#' # Count each Action Unit
#'
#' test_faces %>%
#'   select_faces(starts_with("AU")) %>%
#'   tidy_face(continuous = FALSE, events_sum = "count")

tidy_face <- function(x, events = TRUE, continuous = TRUE, events_sum = "count", ...){


  x <- x %>% # merge all the faces datasets into one
    merge_face()

  list_x <- list() # create a list where to store the three parts: time, events and continuous variables

  events_p <- ifelse(events_sum %in% c("eps", "epm"), "count", events_sum) #create the parameter for the event_summarise functon

  list_x[["time"]] <- time_summarise(x) # length of videos

  if (events == TRUE) { #events summary

    list_x[["events"]] <-  event_summarise(x, as = events_p)

  }


  if(events_sum %in% c("eps", "epm")){

    list_x[["events"]][2:ncol(list_x[["events"]])] <-
      list_x[["events"]][2:ncol(list_x[["events"]])]/list_x[["time"]]$length

  }

  if(events_sum == "epm"){

    list_x[["events"]][2:ncol(list_x[["events"]])] <-
      list_x[["events"]][2:ncol(list_x[["events"]])]*60

  }

  if (continuous == TRUE) { #continuous summary

    list_x[["continuous"]] <- continuous_summarise(x, ...)

  }

  x <- reduce(list_x, left_join)

  return(x)
}

merge_face <- function(x) {
  # convert a faces object into a tidy dataframe, summarising continuous and discrete variables

  for (i in 1:length(x)){ # This loop create per each dataframe a new variable with the video ID

    id.name <- names(x)[[i]] #identifies the video ID
    x[[i]] <- x[[i]] %>% # create the variable
      dplyr::mutate(ID = id.name) %>%
      dplyr::mutate(ID = as.factor(ID)) %>%# convert to factor to use the unsplit function later
      dplyr::select(ID, everything()) # have it as first row
  }

  x <- do.call(rbind, x) # merge together all the videos



  return(x)

}


## some explanation here: The tidyface function is composed of two parts.
# 1. merge_faces merge together all the faces of the list
# 2. two summarise functions (for discrete and continuous vars) summarise all variables

# 3. for running the summarise is necessary to create a string with a list of summarising functions
# that can be chosen by the user

create_funlist_continuous <- function(mean = T, sd = T, median = F, min = F, max = F, mode = F){
  # the list can be expanded in the future or more elegant methods of parsing can be thought

  listfun = list()

  if (mean == TRUE) {
    listfun["mean"] = "mean = ~ mean(x = ., na.rm = T)"
  }

  if (sd == TRUE) {
    listfun["sd"] = "sd = ~ sd(x = ., na.rm = T)"
  }

  if (median == TRUE) {
    listfun["median"] = "median = ~ median(x = ., na.rm = T)"
  }

  if (min == TRUE) {
    listfun["min"] = "min = ~ min(x = ., na.rm = T)"
  }

  if (max == TRUE) {
    listfun["max"] = "max = ~ max(x = ., na.rm = T)"
  }


  funlist <- reduce(listfun, .f = function(x, y)paste(x,y, sep = " , "))
  funlist <- paste("list(", funlist, ")", sep = "")
  return(funlist)
}


### rules to summarise a continuous variable
continuous_summarise <- function(x, ...){

  funlist <- create_funlist_continuous(...)

  x <- x %>%
    dplyr::group_by(ID) %>%
    dplyr::select(-c(timestamp, frame, success)) %>%
    dplyr::select_if(is_continuous) %>%
    dplyr::summarise_all(.funs = eval(parse(text = funlist))) #TODO add a is_frequency condition

  return(x)

}

### recognise a frequency variable

is_frequency <- function(x){ # note, it will read as frequencies also continuous vars with all 0s

  ifelse(mean(levels(as.factor(x)) %in% c(1,0)) == 1, TRUE, FALSE)

}

### recognise a continuous variable

is_continuous <- function(x){ # note, it will read as frequencies also continuous vars with all 0s

  ifelse(is.numeric(x) == TRUE & is_frequency(x) == FALSE, TRUE, FALSE)

}


#' event summarise
#' summarise all the frequency variables
#' @param x the dataframe to summarise
#' @param as "eps", standard, is event per second. "count" is the number of events. "

event_summarise <- function(x, as = "count", ...){

  x <- x %>%
    dplyr::group_by(ID) %>%
    dplyr::select(-(timestamp:success))


  if (as == "ratio") {
    x <- x %>%
      dplyr::summarise_if(is_frequency, .funs = ~ ratio(x = .))

  } else if (as == "count"){

    x <- x  %>%
      dplyr::summarise_if(is_frequency, .funs = ~ count_events(x = ., ...))

  }

  return(x)

}

ratio <- function(x){
  mean(x, na.rm = T)
}

#' time summarise
#' The other information we want to keep is the total time of the video
#' @param x: a faces dataframe
#' @return a dataframe with two variables: the video ID and the length in seconds of each video

time_summarise <- function(x){
  x %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(length = max(timestamp))
}






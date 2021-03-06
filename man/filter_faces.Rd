% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_mutate_select_tidy.R
\name{filter_faces}
\alias{filter_faces}
\title{Filtering faces objects.}
\usage{
filter_faces(faces, ...)
}
\arguments{
\item{faces}{The face object to filter.}

\item{...}{The filtering condition. The function will delete all the rows in all the dataframes
for which the condition is false.}
}
\value{
The function returns a filtered "faces" object
}
\description{
Filters across all the dataframes in a faces object.
}
\examples{

 # Delete unsuccessful trials

 filter_faces(test_faces, success == 1)

 # Take only the first minute of each video analysed

 test_faces \%>\%
 filter_faces(timestamp < 60)
}

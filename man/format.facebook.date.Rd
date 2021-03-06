% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format-facebook-date.R
\name{formatFbDate}
\alias{formatFbDate}
\title{Converts from Facebook date format to R date format}
\usage{
formatFbDate(datestring, format = "datetime")
}
\arguments{
\item{datestring}{Date string in Facebook format}

\item{output}{Either "date", for Date format; or "datetime" for POSIX.}
}
\examples{
\dontrun{
## example of Facebook data
  example <- "2014-02-21T16:22:03+0000"
  dt <- format.facebook.date(example)
}

}
\author{
Pablo Barbera \email{pbarbera@usc.edu}
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_api.R
\name{get_classification}
\alias{get_classification}
\title{Get classification series}
\usage{
get_classification(localId, lang = "fi", as_named_vector = FALSE)
}
\arguments{
\item{localId}{character, localId of the required correspondence table}

\item{lang}{\code{"fi"}, \code{"en"}, or \code{"sv"}, desired language.
Defaults to \code{"fi"}.}

\item{as_named_vector}{logical, whether to return the object as a named vector rather
than data.frame. Defaults to \code{FALSE}.}
}
\value{
a data.frame, a classification series
}
\description{
A wrapper for \code{access_API} to get classifications.
}
\examples{

  localId <- "siviiliasiat_1_20140101"
  get_classification(localId)

}

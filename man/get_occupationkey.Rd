% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_occupationkey.R
\name{get_occupationkey}
\alias{get_occupationkey}
\title{Get an occupation name-code key}
\usage{
get_occupationkey(
  code_level = NULL,
  year = 2010,
  lang = "fi",
  as_named_vector = FALSE
)
}
\arguments{
\item{code_level, }{disaggregation level}

\item{year, }{year of the key}

\item{lang, }{\code{"fi"}, \code{"sv"} or \code{"en"}. Language of the key.
Defaults to \code{"fi"}.}

\item{as_named_vector, }{logical, whether to return a named vector rather than
a data.frame. Defaults to \code{FALSE}.}
}
\value{
data.frame with occupation codes and names
}
\description{
Get an occupation name-code key
}
\examples{

   get_occupationkey(1)
   get_occupationkey(1, as_named_vector = TRUE)

}

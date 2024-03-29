% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_api.R
\name{access_API}
\alias{access_API}
\title{API interface}
\usage{
access_API(
  localId = NULL,
  content = "data",
  classification_service = NULL,
  lang = "fi"
)
}
\arguments{
\item{localId}{character, local ID of the correspondence table or classification}

\item{content}{character, \code{"data"} or \code{"url"} determines the content of query}

\item{classification_service}{character, \code{"correspondenceTable"} or
\code{"classifications"}. Determines the classification service used.}

\item{lang}{\code{"fi"}, \code{"en"}, or \code{"sv"}, desired language.
Defaults to \code{"fi"}.}
}
\value{
data.frame either the correspondence table or its url depending on argument \code{content}.
}
\description{
Given a local Id of a correspondence table or classification, gets the data
and transforms it into a data.frame. For internal use of the package.
}
\details{
Currently supported classification services are 'classifications' and
'correspondenceTables'. For more information on classification services,
see \url{https://www.stat.fi/en/luokitukset/info/}.
}
\examples{

  localId <- "kunta_1_20200101\%23seutukunta_1_20200101"
  df <- access_API(localId, content = "data")
  access_API(localId, content = "url")

}

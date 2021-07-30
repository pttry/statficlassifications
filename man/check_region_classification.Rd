% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_for_checking_region_classifications.R
\name{check_region_classification}
\alias{check_region_classification}
\title{Check region classifications}
\usage{
check_region_classification(
  region_names,
  region_codes,
  year = NULL,
  lang = "fi",
  offline = TRUE
)
}
\arguments{
\item{region_names}{a vector of standard region names}

\item{region_codes}{a vector of standard region codes}

\item{year}{double or character, year of classification}

\item{lang}{\code{fi}, \code{sv} or \code{en}. Language of the input name. Defaults to \code{fi}.}

\item{offline}{logical, whether works offline with package data. Defaults to \code{TRUE}.}
}
\value{

}
\description{
Check region classifications
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_to_deal_with_region_codes_and_names.R
\name{names_to_codes}
\alias{names_to_codes}
\alias{names_to_codes_vct}
\alias{names_to_codes_fct}
\title{Change region names to region codes}
\usage{
names_to_codes(
  x,
  year = NULL,
  lang = "fi",
  offline = TRUE,
  region_level = NULL
)

names_to_codes_vct(
  x,
  year = NULL,
  lang = "fi",
  offline = TRUE,
  region_level = NULL
)

names_to_codes_fct(
  x,
  year = NULL,
  lang = "fi",
  offline = TRUE,
  region_level = NULL
)
}
\arguments{
\item{x}{character vector of factor of region codes.}

\item{year}{integer, the year of the applied classification key.}

\item{lang, }{\code{fi}, \code{sv} or \code{en}. Input language. Defaults to \code{fi}.}

\item{offline}{logical, whether works offline with package data. Defaults to \code{TRUE}.}

\item{region_level}{character, region level of input codes, optional.}
}
\value{
vector or factor
}
\description{
Change region names to region codes
}
\section{Functions}{
\itemize{
\item \code{names_to_codes_vct}: Change region names to region codes

For internal use.

\item \code{names_to_codes_fct}: #' Recode name to codes in factors

For internal use.
}}

\examples{

  v <- c("KOKO MAA", "Humppila", "Ã…lands skÃ¤rgÃ¥rd", "Satakunta")
  names_to_codes(v)
  names_to_codes("Kajaani", region_level = "seutukunta")
  names_to_codes(c("KOKO MAA", "Kajaani"), region_level = "seutukunta")
  f <- factor(c("KOKO MAA", "Humppila","Ã…lands skÃ¤rgÃ¥rd", "Satakunta"))
  names_to_codes(f)

}

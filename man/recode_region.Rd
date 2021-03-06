% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_to_recode_regions.R
\name{recode_region}
\alias{recode_region}
\title{Recode regional variables}
\usage{
recode_region(
  data,
  from_orig,
  from,
  to,
  year = NULL,
  leave = FALSE,
  offline = TRUE
)
}
\arguments{
\item{data}{data.frame, the input data that contains the regional variable.}

\item{from}{character, the regional variable name in the input data.}

\item{to}{character, the desired target in the classification key}

\item{year}{character or numeric, the year of the}

\item{leave}{logical, whether to leave the original 'from' variable in to the returing
data.frame. Defaults to FALSE.}
}
\value{
data.frame
}
\description{
Recodes regional variables by e.g. changing kunnat to seutukunnat or changing names to codes
and vice versa. Uses the 'get_regionkey'-function.
}
\examples{

# Replace municipality names by their codes

    # Generate random municipal data
       data <- get_regionkey("seutukunta") \%>\%
               dplyr::select(seutukunta_code) \%>\%
               dplyr::mutate(values = rnorm(dplyr::n()))
    # Recode
    recode_region(data, "kunta_name", "kunta_name", "kunta_code")

# Add seutukunnat to municipal data

    recode_region(data, "kunta_name", "kunta_name", "seutukunta_name", year = 2020, leave = TRUE)

}

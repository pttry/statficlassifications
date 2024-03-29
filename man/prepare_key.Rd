% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/key_recode.R
\name{prepare_key}
\alias{prepare_key}
\title{Prepare key for recoding with key}
\usage{
prepare_key(
  x = NULL,
  key,
  from = NULL,
  to = NULL,
  x_name = deparse(substitute(x)),
  by = "names",
  add = FALSE
)
}
\arguments{
\item{x}{vector}

\item{key}{named vector, data.frame or a list}

\item{from}{source column in ky}

\item{to}{target column in key}

\item{x_name}{name of the source column to be decoded}

\item{by}{\code{"names"}, \code{"values"} or \code{"all_values"}. Controls
how x is matched to from column in key}
}
\value{
list containing the prepared key, from-column name and to-column
name(s).
}
\description{
If argument from and to are given, selects these columns from the key. If
these arguments are not given, chooses the most suitable from-column from the
key given either by the name of x-argument or by the values of x-argument.
}
\details{
key can be given either as a named vector, data.frame or a list. List is i
interpreted as a list of keys and if a list is given, a suitable key from
the list is found either by the name of x-argument or by the values of
x-argument.

Also ensures that the prepared key does not have duplicate rows. Duplicate
rows may occur when keys with multiple levels (e.g. regional levels) are
are used such that the from-column is some higher level.
}
\examples{

key <- data.frame(var1 = letters[1:4],
                  var2 = c("first letter",
                           "second letter",
                           "third letter",
                           "fourth letter"),
                  var3 = 1:4)

prepare_key(key = key, from = "var1", to = "var3")


   # Consider iris data
   data("iris")

   key <- data.frame(Species = c("setosa", "versicolor", "virginica"),
                     common_name = c("bristle-pointed iris",
                      "harlequin blueflag", "Virginia blueflag"))
   prepare_key(iris, key, by = "names")

   # Things are often not so happily that the column names in data and the
   # key would match.

   names(key)[1] <- "Genus"
   prepare_key(iris, key, by = "values")

}

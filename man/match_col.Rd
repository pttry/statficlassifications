% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/key_recode.R
\name{match_col}
\alias{match_col}
\alias{match_col_by_values}
\alias{match_col_by_all_values}
\alias{match_col_by_names}
\title{Match columns to list elements}
\usage{
match_col(x, y, x_name = deparse(substitute(x)), by = "names")

match_col_by_values(x, y, x_name = deparse(substitute(x)))

match_col_by_all_values(x, y, x_name = deparse(substitute(x)))

match_col_by_names(x, y, x_name = deparse(substitute(x)))
}
\arguments{
\item{x}{vector, factor or data.frame.}

\item{y}{data.frame where matcheds are found.}

\item{x_name}{name of input (column) to tell which is the from column in key.}

\item{by}{\code{"names"} or \code{"values"} whether to match columns by their names or
by the values they contain.}
}
\value{
list with an element for each column in x. Each element of this list
contains all a vector with the names of columns that where matched in y.
}
\description{
Find columns in list that match vector by values or by names. Used to automatically
detect the columns in data.frame that should be recoded given a key.
}
\details{
For each column of x, find the columns in y that has a variable with the same
classification as the column is x has.

by_values: find the column in y that contains most elements of (column of) x to
map the coding of (column of) x to (a column of) y that has the same coding.

by_names: find the column in y that has the same name as (a column of) x to map the
coding of (column of) x to (a column of) y that has the same coding.
}
\examples{

v <- c("a", "b", "a", "c", "b", "b")
f <- factor(v)
df <- data.frame(var1 = v, y = rnorm(6))

key <- data.frame(var1 = letters[1:4],
                  var2 = c("first letter",
                           "second letter",
                           "third letter",
                           "fourth letter"))

 match_col(v, key, by = "values")
 var1 <- v
 match_col(var1, key)
 match_col(f, key, by = "values")
 match_col(df, key)
 match_col(df, key, by = "values")

key <- data.frame(var1 = letters[1:4],
                  var3 = letters[4:1],
                 var2 = c("first letter",
                          "second letter",
                          "third letter",
                          "fourth letter"))
 match_col(v, key, by = "values")
 match_col(df, key, by = "names")
 match_col(df, key, by = "values")


 # Given the iris data set. Find in that data set the column that contains
 # the same values as vector v and thus likely describes the same variable
 data(iris)
 v <- c("setosa", "versicolor")
 match_col(v, iris, by = "values")

 # Now suppse there is a key that maps the Latin Species name to a common
 # name
 key <- data.frame(Species = c("setosa", "versicolor", "virginica"),
                   common_name = c("bristle-pointed iris",
                            "harlequin blueflag", "Virginia blueflag"))

 # We find for each column in iris, a column in the key that has data on the
 # same variable as in the column. Here by names:
 match_col(iris, key, by = "names")

 # But say the key has a different variable name even though the variable is
 # the same:
 names(key)[1] <- "Genus"
 match_col(iris, key, by = "values")

 # Sometime's inputs do not have clear names to use in mapping to key
 match_col(iris$Species, key, by = "values")

}

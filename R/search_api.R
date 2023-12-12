#' Search for classification keys
#'
#' Searches and browses available classification keys / correspondence tables in
#' the correspondenceTables classification service
#'
#' @param ... character, search words.
#' @param as_localId logical, whether returns the localID of the found table. Defaults to `FALSE`.
#'
#' @return character vector
#'
#' @import dplyr
#' @export
#'
#' @examples
#'
#' # Browse all keys
#'    search_keys()
#'
#' # Search for keys with search word "maakunta"
#'    search_keys("maakunta")
#'
#' # Search for keys from kunta to maakunta for year 2016
#'    search_keys("kunta", "maakunta", 2016)
#'
search_keys <- function(..., as_localId = FALSE) {

  # Get a list of all correspondence table urls and create a data.frame that isolates the components
  # of the endpoints
    results <- suppressMessages(urls_as_localId_df(get_url(classification_service = "correspondenceTables")))
    searchterms <- unlist(list(...))

  # Filter results by the searchterms
    results <- filter_results(results, searchterms)

  # Interrupt if nothing found.
    if(dim(results)[1] == 0) return(message("No search results!"))

  # Format output.
    if(as_localId) {
      output <- create_localId_name(source = results$source, target = results$target,
                                    year1 = results$year1, year2 = results$year2,
                                    date1 = results$date1, date2 = results$date2,
                                    nro1 = results$nro1, nro2 = results$nro2)
    } else {
        output <- paste(results$source,
                           results$year1,
                           "->",
                           results$target,
                           results$year2, sep = " ")
    }

  # Return.
  unique(output)
}

#' Search for classifications
#'
#' Searches and browses available classifications in the Classifications
#' classification service
#'
#' @param ... character, search words.
#' @param as_localId logical, whether returns the localID of the found table. Defaults to `FALSE`.
#'
#' @return vector of classifications
#' @export
#'
#' @examples
#'
#'   search_classifications("ammatti")
#'   search_classifications("ammatti", year = 2021, as_localId = TRUE)
#'
search_classifications <- function(..., as_localId = FALSE){

  # Get a list of all correspondence table urls and create a data.frame that isolates the components
  # of the endpoints
  results <- suppressMessages(urls_as_localId_df(get_url(classification_service = "classifications")))
  searchterms <- unlist(list(...))

  # Filter results by the searchterms
  results <- filter_results(results, searchterms)

  # Interrupt if nothing found.
  if(dim(results)[1] == 0) return(stop("No search results!"))

  # Format output
  if(as_localId) {
      output <- paste0(results$series, results$nro, results$year, results$date)
  } else {
      output <- paste(results$series, results$year)
  }

  # Return.
  unique(output)
}

#' Filter search results
#'
#' @param results character vector, the search field
#' @param searchterms character vector search terms
#'
#' @return character vector of elements of input that match the search terms
#' @export
#'
filter_results <- function(results, searchterms) {

  # Filter results by the searchterms
  if(length(searchterms) > 0) searchterms <- unlist(strsplit(searchterms, " "))
  results[sapply(1:dim(results)[1], function(i) { all(searchterms %in% c(results[i,])) }), ]

}

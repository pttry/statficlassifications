#' Search for classification keys
#'
#' Searches and browses available classification keys / correspondence tables in
#' the correspondenceTables classification service
#'
#' @param ... character, search words.
#' @param as_localId logical, whether returns the localID of the found table. Defaults to \code{FALSE}.
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
#'
search_keys <- function(..., as_localId = FALSE) {

  # Get a list of all correspondence table urls and create a data.frame that isolates the components
  # of the endpoints
  results <- suppressMessages(urls_as_localId_df(get_url(classification_service = "correspondenceTables")))

  # Filter results by the searchterms
  searchterms <- unlist(list(...))
  results <-  results[sapply(1:dim(results)[1], function(i) { all(searchterms %in% c(results[i,])) }), ]

  # Interrupt if nothing found.
  if(dim(results)[1] == 0) {
    return("No search results!")
  }

  # Format output.
  output <- character(dim(results)[1])
  if(as_localId) {
    for(i in 1:dim(results)[1]) {
      output[i] <- create_localId_name(input_vector = results[i,])
    }
  } else {
    for(i in 1:dim(results)[1]) {
      output[i] <- paste(results[i, "source"],
                         results[i, "year1"],
                         "->",
                         results[i, "target"],
                         results[i, "year2"], sep = " ")
    }
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
#' @param as_localId logical, whether returns the localID of the found table. Defaults to FALSE.
#'
#' @return
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

  # Filter results by the searchterms
  searchterms <- unlist(list(...))
  results <-  results[sapply(1:dim(results)[1], function(i) { all(searchterms %in% c(results[i,]))}), ]

  # Interrupt of nothing found.
  if(dim(results)[1] == 0) {
    return("No search results!")
  }

  # Format output
  output <- character(dim(results)[1])
  if(as_localId) {
    for(i in 1:dim(results)[1]) {
      output[i] <- paste0(results$series[i], results$nro[i], results$year[i], results$date[i])
    }
  } else {
    for(i in 1:dim(results)[1]) {
      output[i] <- paste(results$series[i], results$year[i])
    }
  }

  # Return.
  unique(output)
}

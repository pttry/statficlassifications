#' Search for classifications
#'
#' @param ... character, search words.
#' @param year character or numerical, search for specific years.
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
search_classifications <- function(...,
                          year = NULL,
                          as_localId = FALSE){

  results <- urls_as_localId_df(get_url(classification_service = "classifications"))
  search_year <- year

  # Filter results by the searchterms

  searchterms <- unlist(list(...))

  if(length(searchterms) > 0) {
     results <- dplyr::filter(results, series %in% searchterms)
  }
  if(!is.null(year)) {
    results <- dplyr::filter(results, year == search_year)
  }

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

  output
}

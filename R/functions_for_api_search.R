#' Search for classification keys
#'
#' A function to search and browse available classification keys / correspondence tables.
#'
#' @param ... character, search words.
#' @param search_source logical, whether search only among the sources.
#' @param search_target logical, whether search only among the targets.
#' @param source_searchterm character, a search word to use in search only among the sources.
#' @param target_searchterm character, a search word to use in search only among the targets.
#' @param year character or numerical, search for specific years.
#' @param as_localID logical, whether returns the localID of the found table. Defaults to FALSE.
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
#'    search_keys("maakunta)
#'
#' # Search for keys that have "maakunta" as source
#'    search_keys(search_source = "maakunta")
#'
#' # Search for keys that map "kunta" to "maakunta"
#'    search_keys(source_searchterm = "kunta", target_searchterm = "maakunta")
#'
#' # Search for keys that map "kunta" to "maakunta" for year 2016 and print as localId
#'   search_keys(source_searchterm = "kunta", target_searchterm = "maakunta",
#'               year = 2016, as_localId = TRUE)
#'
#'
search_keys <- function(...,
                        search_source = FALSE,
                        search_target = FALSE,
                        source = NULL,
                        target= NULL,
                        year = NULL,
                        as_localId = FALSE) {

  source_searchterm <- source
  target_searchterm <- target

  # Get a list of all correspondence table urls and create a data.frame that isolates the components
  # of the endpoints

  results <- urls_as_localId_df(get_url(classification_service = "correspondenceTables"))

  # Filter results by the searchterms

  searchterms <- unlist(list(...))

   if(length(searchterms) > 0) {
     results_temp <- data.frame()
     for(word in searchterms) {
       match_indicator_source <- NULL
       match_indicator_target <- NULL

       if(!search_target) {
          match_indicator_source <- sapply(results$source, grepl, pattern = word)
       }
       if(!search_source) {
          match_indicator_target <- sapply(results$target, grepl, pattern = word)
       }
       results_temp <- rbind(results_temp,
                             results[match_indicator_source,],
                             results[match_indicator_target,])
     }
     results <- results_temp
   }

   if(!is.null(year)) {
    results <- dplyr::filter(results, year1 == year | year2 == year)
   }

   if(!is.null(source_searchterm)) {
     results <- dplyr::filter(results, source_searchterm == source)
   }

  if(!is.null(target_searchterm)) {
    results <- dplyr::filter(results, target_searchterm == target)
  }

   if(dim(results)[1] == 0) {
     return("No search results!")
   }

  # Format output

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

   unique(output)
}



#' Search for classifications
#'
#' A function to search and browse available classifications.
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

  unique(output)
}


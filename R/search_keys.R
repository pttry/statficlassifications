#' Search for classification keys
#'
#' A function to search and browse Statistics Finland Classifications correspondence tables.
#'
#' @param ... character, search words.
#' @param search_source logical, whether search only among the sources.
#' @param search_target logical, whether search only among the targets.
#' @param searchword_source character, a search word to use in search only among the sources.
#' @param searchword_target character, a search word to use in search only among the targets.
#' @param year character or numerical, search for specific years.
#' @param as_localID logical, whether returns the localID of the found table. Defaults to FALSE.
#'
#' @return character vector
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
#'    search_keys(search_source = "maakunta)
#'
#' # Search for keys that map "kunta" to "maakunta"
#'    search_keys(searchword_source = "kunta", searchword_target = "maakunta")
#'
#' # Search for keys that map "kunta" to "maakunta" for year 2016 and print as localID
#'   search_keys(searchword_source = "kunta", searchword_target = "maakunta",
#'               year = 2016, as_localID = TRUE)
#'
#'
search_keys <- function(...,
                        search_source = FALSE,
                        search_target = FALSE,
                        searchword_source = NULL,
                        searchword_target = NULL,
                        year = NULL,
                        as_localID = FALSE) {

  # Get a list of all correspondence table urls and create a data.frame that isolates the components
  # of the endpoints

  urls <- get_url()
  urls <- as.data.frame(sapply(urls, stringr::str_remove,
                               paste0("https://data.stat.fi/api/classifications/v2/correspondenceTables/")))
  nros <- as.data.frame(matrix(unlist(lapply(urls, stringr::str_extract_all, "_\\d+_")), ncol = 2, byrow = TRUE))
  names(nros) <- paste0("nro", 1:2)
  results <- tidyr::separate(urls, url, c("source", "temp_var", "date2"), sep = "_\\d+_") %>%
             tidyr::separate(temp_var, c("date1", "target"), sep = "#") %>%
             dplyr::mutate(year1 = substring(date1, 1,4),
                           date1 = substring(date1, 5,8),
                           year2 = substring(date2, 1,4),
                           date2 = substring(date2, 5,8))
  results <- cbind(results, nros)

  # Filter results by the searchwords

  searchwords <- unlist(list(...))

   if(length(searchwords > 0)) {
     results_temp <- data.frame()
     for(word in searchwords) {
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

   if(!is.null(searchword_source)) {
     results <- dplyr::filter(results, grepl(searchword_source, source))
   }

  if(!is.null(searchword_target)) {
    results <- dplyr::filter(results, grepl(searchword_target, target))
  }

   if(dim(results)[1] == 0) {
     return("No search results!")
   }

  # Format output

   output <- character(dim(results)[1])
   if(as_localID) {
     for(i in 1:dim(results)[1]) {
     output[i] <- create_localID_name(input_vector = results[i,])
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

   output
}


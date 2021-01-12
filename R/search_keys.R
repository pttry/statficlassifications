#' Search for classification keys
#'
#' A function to search and browse Statistics Finland Classifications correspondence tables.
#'
#' @param ... character, the search words.
#' @param search_source logical, whether search only among the sources.
#' @param search_target logical, whether search only among the targets.
#' @param year character or numerical, search for specific years.
#' @param return character vector
#' @param as_localID logical, whether returns the localID of the found table. Defaults to FALSE.
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Browse all keys
#'    search_keys()
#'
search_keys <- function(...,
                        search_source = FALSE,
                        search_target = FALSE,
                        searchword_source = NULL,
                        searchword_target = NULL,
                        year = NULL,
                        as_localID = FALSE) {

  url <- "https://data.stat.fi/api/classifications/v2/correspondenceTables"
  endpoints <- as.data.frame(
             jsonlite::fromJSON(
                  rawToChar(
                    httr::GET(url,query = list(content = "url", meta = "min"))$content)
                  )
             )
  endpoints <- as.data.frame(sapply(endpoints, stringr::str_remove, paste0(url, "/")))
  names(endpoints) <- "endpoint"
  nros <- as.data.frame(matrix(unlist(lapply(endpoints, stringr::str_extract_all, "_\\d+_")), ncol = 2, byrow = TRUE))
  names(nros) <- paste0("nro", 1:2)
  results <- tidyr::separate(endpoints, endpoint, c("source", "temp_var", "date2"), sep = "_\\d+_") %>%
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
     output[i] <- paste0(results[i, "source"],
            results[i, "nro1"],
            paste0(paste0(results[i, "year1"], results[i, "date1"]),"%23", results[i, "target"]),
            results[i, "nro2"],
            paste0(paste0(results[i, "year2"], results[i, "date2"])))
     }
    } else {
       for(i in 1:dim(results)[1]) {
         output[i] <- paste(results[i, "source"],
                            results[i, "year1"],
                            "->",
                            results[i, "target"],
                            results[i, "year1"], sep = " ")
       }
     }

   output
}







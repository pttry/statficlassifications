#' Search for classification keys
#'
#' A function to search and browse Statistics Finland Classifications correspondence tables.
#'
#' @param searchword_source
#' @param searchword_target
#' @param year
#' @param return
#' @param as_localID logical, whether returns the localID of the found table. Defaults to FALSE.
#'
#' @return
#' @export
#'
#' @examples
search_correspondencetables <- function(searchword_source = NULL,
                                        searchword_target = NULL,
                                        searchyear = NULL, return, as_localID = FALSE) {

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

   output <- dplyr::select(results, source, target) %>%
             dplyr::filter(grepl(searchword_source, source) | grepl(searchword_target,target))

   if(!is.null(year)) {
     output <- dplyr::filter(grepl(as.character(year), year1) | grepl(as.character(year), year2))
   }

   results2 <- character(dim(results)[1])
   if(as_localID) {
     for(i in 1:dim(results)[1]) {
     results2[i] <- paste0(results[i, "source"],
            results[i, "nro1"],
            paste0(paste0(results[i, "year1"], results[i, "date1"]),"%23", results[i, "target"]),
            results[i, "nro2"],
            paste0(paste0(results[i, "year2"], results[i, "date2"])))

     }

   }
}





}

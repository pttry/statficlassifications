#' API interface
#'
#' Given a local ID of a correspondence table fetches the table and reads it.
#'
#' @param localID character local ID of the required correspondence table
#' @param content, character either "data" or "url" whether the content of the query
#'    is data or url.
#'
#' @return data.frame
#' @export
#'
#' @examples
access_API <- function(localID = NULL, content = "data") {

  if(!(content %in% c("url", "data"))) {
    stop("Argument 'content' has to be either 'data' or 'url'.")
  }

  if(!is.null(localID)) {
    localID <- paste0("/", localID)
  }

  # Set url either ot get url or content
  url <- paste0("https://data.stat.fi/api/classifications/v2/correspondenceTables",
                localID,
                ifelse(content == "data", "/maps", ""))

  # access API and return
  resp <- httr::GET(url, query = list(content = content, meta = "min"))
  cont <- httr::content(resp, "text", encoding = "UTF-8")

  as.data.frame(jsonlite::fromJSON(cont))
}


#' Get urls from API
#'
#' A wrapper for \code{access_API} function to get url.
#'
#' @param localID, character, local ID of the required correspondence table
#'
#' @return character,
#' @export
#'
#' @examples
get_url <- function(localID = NULL) {
  url <- access_API(localID, content = "url")
  names(url) <- "url"
  url
}


#' Get data from API
#'
#' A wrapper for \code{access_API} to get data.
#'
#' @param localID, character, local ID of the required correspondence table
#'
#' @return data.frame
#' @export
#'
#' @examples
get_key <- function(localID, print_key_name = TRUE) {

  if(length(localID) > 1) {
    stop("Multiple localIDs! This function currently gives you only one key at the time.")
  }

  key <- access_API(localID, content = "data")
  text <- unique(key$correspondenceTable$correspondenceTableTexts)
  key <- data.frame(source_code = key$sourceItem$code,
                    source_name = unlist(lapply(key$sourceItem$classificationItemNames, '[', "name")),
                    target_code = key$targetItem$code,
                    target_name = unlist(lapply(key$targetItem$classificationItemNames, '[', "name")))

  if(print_key_name) {message(text)}
  key
}


#' Get the year of the newest correspondence table
#'
#' @return double, the year of the newest correspondence table
#' @export
#'
#' @examples
get_latest_year <- function() {

  urls <- get_url()
  urls <- as.data.frame(sapply(urls, stringr::str_remove,
                               paste0("https://data.stat.fi/api/classifications/v2/correspondenceTables/")))
  results <- tidyr::separate(urls, url, c("source", "temp_var", "date2"), sep = "_\\d+_") %>%
    tidyr::separate(temp_var, c("date1", "target"), sep = "#") %>%
    dplyr::mutate(year1 = as.double(substring(date1, 1,4)),
                  year2 = as.double(substring(date2, 1,4)))
  max(c(results$year1, results$year2))
}

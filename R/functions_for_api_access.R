#' API interface
#'
#' Given a local ID of a correspondence table, gets the table and transforms it into a data.frame.
#' For internal use.
#'
#' @param localID character local ID of the required correspondence table
#' @param content, character either "data" or "url" whether the content of the query
#'    is data or url.
#'
#' @return data.frame either the correspondence table or its url depending on argument \code{content}.
#' @export
#'
#' @examples
#'
#'   localId <- "kunta_1_20200101%23seutukunta_1_20200101"
#'   access_API(localId, content = "data")
#'   access_API(localId, content = "url")
#'
access_API <- function(localId = NULL, content = "data") {

  if(!(content %in% c("url", "data"))) {
    stop("Argument 'content' has to be either 'data' or 'url'.")
  }

  # If localId provided, prepend with "/" to build url to the endpoint.
  if(!is.null(localId)) {
    localId <- paste0("/", localId)
  }

  # Set url either to get url or content
  url <- paste0("https://data.stat.fi/api/classifications/v2/correspondenceTables",
                localId,
                ifelse(content == "data", "/maps", ""))

  # access API and return
  resp <- httr::GET(url, query = list(content = content, meta = "min"))
  cont <- httr::content(resp, "text", encoding = "UTF-8")

  as.data.frame(jsonlite::fromJSON(cont))
}


#' Get url from API
#'
#' A wrapper for \code{access_API} function to get url.
#'
#' @param localId, character, localId of the required correspondence table
#'
#' @return character, url of the provided localId.
#' @export
#'
#' @examples
#'
#'    localId <- "kunta_1_20200101%23seutukunta_1_20200101"
#'    get_url(localId)
#'
get_url <- function(localId = NULL) {

  url <- access_API(localId, content = "url")
  names(url) <- "url"
  url
}


#' Get data from API
#'
#' A wrapper for \code{access_API} to get data.
#'
#' @param localID, character, local ID of the required correspondence table
#' @param print_key_name, whether prints the long name of the correspondence table got.
#'
#' @return data.frame, the key of the provided localId.
#' @export
#'
#' @examples
#'
#'    localId <- "kunta_1_20200101%23seutukunta_1_20200101"
#'    get_key(localId)
#'
get_key <- function(localId, print_key_name = TRUE) {

  if(length(localId) > 1) {
    stop("Multiple localIds! This function currently gives you only one key at the time.")
  }

  key <- access_API(localId, content = "data")
  text <- unique(key$correspondenceTable$correspondenceTableTexts)
  key <- data.frame(source_code = key$sourceItem$code,
                    source_name = unlist(lapply(key$sourceItem$classificationItemNames, '[', "name")),
                    target_code = key$targetItem$code,
                    target_name = unlist(lapply(key$targetItem$classificationItemNames, '[', "name")))

  if(print_key_name) {message(text)}
  key
}


#' Get the year of the latest correspondence table
#'
#' @return double, the year of the newest correspondence table
#' @export
#'
#'
#' @examples
#'
#'  get_latest_year()
#'
get_latest_year <- function(offline = TRUE) {

 if(offline) {
   sys_current_year <- as.double(substring(Sys.Date(), 1,4))
   sys_current_month <- as.double(substring(Sys.Date(), 6,7))
   if(sys_current_month == 1) {
     warning("In January, the offline latest year is the last year in case no fresh keys updated.")
     return(sys_current_year - 1)
   } else {
     sys_current_year
   }

 } else {
     urls <- get_url()
     results <- urls_as_localId_df(urls)
     as.double(max(c(results$year1, results$year2)))
 }
}




#' Transform a list of urls into a data.frame that separates the relevant information of each localId.
#' For internal use.
#'
#' @param urls, a list of characters, urls of localIds..
#'
#' @return data.frame containing the relevant information of each localId correponding to the input urls.
#' @import dplyr
#' @export
#'
#' @examples
urls_as_localId_df <- function(urls) {

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
  cbind(results, nros)
}

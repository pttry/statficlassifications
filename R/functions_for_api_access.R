#' API interface
#'
#' Given a local ID of a correspondence table or classification, gets the data
#' and transforms it into a data.frame. For internal use of the package.
#'
#' Currently supported classification services are 'classifications' and
#' 'correspondenceTables'. For more information on classification services,
#' see <https://www.stat.fi/en/luokitukset/info/>.
#'
#' @param localID character, local ID of the correspondence table or classification
#' @param content, character, either "data" or "url" whether the content of the query
#'    is data or url.
#' @param classification_service character, either 'correspondenceTable' or
#'    'classifications'. Determines the classification service used.
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
access_API <- function(localId = NULL, content = "data", classification_service = NULL) {

  if(is.null(localId) & is.null(classification_service)) {
    stop("Please provide either a localId or a classification_service.")
  }

  if(is.null(classification_service)) {
      classification_service <- find_classification_service(localId)
  }

  if(!(classification_service %in% c("classifications", "correspondenceTables"))) {
    stop("Unknown classification service.")
  }

  if(!(content %in% c("url", "data"))) {
    stop("Argument 'content' has to be either 'data' or 'url'.")
  }

  # If localId provided, prepend with "/" to build url to the endpoint.
  if(!is.null(localId)) {
    localId <- paste0("/", localId)
  }

  # Set url either to get url or content

  url_ends <- c("classifications" = "/classificationItems",
                     "correspondenceTables" = "/maps")

  url <- paste0("https://data.stat.fi/api/classifications/v2/",
                classification_service,
                localId,
                ifelse(content == "data", url_ends[classification_service], ""))

  # access API and return
  resp <- httr::GET(url, query = list(content = content, meta = "min"))
  cont <- httr::content(resp, "text", encoding = "UTF-8")

  as.data.frame(jsonlite::fromJSON(cont))
}


#' Get url from API
#'
#' A wrapper for \code{access_API} function to get url.
#'
#' @param localId, character, localId of the required table.
#'
#' @return character, url of the provided localId.
#' @export
#'
#' @examples
#'
#'    localId <- "kunta_1_20200101%23seutukunta_1_20200101"
#'    get_url(localId, classification_service = "correspondenceTables")
#'
get_url <- function(localId = NULL, classification_service = NULL) {

  if(is.null(localId) & is.null(classification_service)) {
    stop("Please provide either a localId or a classification_service.")
  }

  url <- access_API(localId, content = "url", classification_service = classification_service)
  names(url) <- "url"
  url
}


#' Get classification key from API
#'
#' A wrapper for \code{access_API} to get data of classification keys.
#'
#' @param localID, character, local ID of the required correspondence table
#' @param print_key_name, whether prints the long name of the correspondence table.
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

  key <- access_API(localId, content = "data", classification_service = "correspondenceTables")
  text <- unique(key$correspondenceTable$correspondenceTableTexts)
  key <- data.frame(source_code = key$sourceItem$code,
                    source_name = unlist(lapply(key$sourceItem$classificationItemNames, '[', "name")),
                    target_code = key$targetItem$code,
                    target_name = unlist(lapply(key$targetItem$classificationItemNames, '[', "name")))

  if(print_key_name) {message(text)}
  key
}

#' Get classification series from API
#'
#' A wrapper for \code{access_API} to get data of classifications.
#'
#' @param localId character, local ID of the required correspondence table
#' @param print_series_name  whether prints the long name of the classification series.
#'
#' @return
#' @export
#'
#' @examples
#'
#'   localId <- "siviiliasiat_1_20140101"
#'   get_classification(localId)
#'
get_classification <- function(localId, print_series_name = TRUE) {

  if(length(localId) > 1) {
    stop("Multiple localIds! This function currently gives you only one series at the time.")
  }

  series <- access_API(localId, content = "data", classification_service = "classifications")
  text <- unlist(unique(series$classification$classificationName))["name"]
  series <- data.frame(code = series$code,
                    name = unlist(lapply(series$classificationItemNames, '[', "name")))

  if(print_series_name) {message(text)}
  series
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
     urls <- get_url(classification_service = "correspondenceTable")
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

  if(grepl("v2/correspondenceTables", urls)) {

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
  output <- cbind(results, nros)

  } else if(grepl("v2/classifications", urls)) {

    urls <- as.data.frame(sapply(urls, stringr::str_remove,
                                 paste0("https://data.stat.fi/api/classifications/v2/classifications/")))
    nro <- as.data.frame(matrix(unlist(lapply(urls, stringr::str_extract_all, "_\\d+_")), ncol = 1, byrow = TRUE))
    names(nro) <- "nro"
    results <- tidyr::separate(urls, url, c("series", "date1"), sep = "_\\d+_") %>%
      dplyr::mutate(year = substring(date1, 1,4),
                    date = substring(date1, 5,8))
    output <- cbind(results, nro)
  }
  output
}


#' Finds a classification service for a localId
#'
#' Given a localId finds whether the localId is a localId in classifications or correspondenceTables
#' classification services. For internal use.
#'
#' @param localId character
#'
#' @return
#' @export
#'
#' @examples
#'
#'     find_classification_service(search_classifications(as_localId = TRUE)[1])
#'
find_classification_service <- function(localId) {

  if(length(localId) > 1) {
    stop("Multiple localIds! This function currently gives you only one series at the time.")
  }

  indicator <- c(localId %in% search_series(as_localId = TRUE),
                 localId %in% search_keys(as_localId = TRUE))

  if(all(!indicator)) {stop("Classification service not found.")}

  c("classifications", "correspondenceTables")[indicator]

}

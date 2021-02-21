#' API interface
#'
#' Given a local Id of a correspondence table or classification, gets the data
#' and transforms it into a data.frame. For internal use of the package.
#'
#' Currently supported classification services are 'classifications' and
#' 'correspondenceTables'. For more information on classification services,
#' see <https://www.stat.fi/en/luokitukset/info/>.
#'
#' @param localId character, local ID of the correspondence table or classification
#' @param content, character, \code{"data"} or \code{"url"} determines the content of query
#' @param classification_service character, \code{"correspondenceTable"} or
#'    \code{"classifications"}. Determines the classification service used. See Details for
#'    information on the classification services.
#' @param lang, \code{"fi"}, \code{"en"}, or \code{"sv"}, language required.
#'    Defaults to \code{"fi"}.
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
access_API <- function(localId = NULL,
                       content = "data",
                       classification_service = NULL,
                       lang = "fi") {

  # Test if neither localId nor classification_service is given. If not, give
  # an error as the function does not know what to access.
    if(is.null(localId) & is.null(classification_service)) {
      stop("Please provide either a localId or a classification_service.")
    }

  # If classification service is not given, use the given localId to determine it.
    if(is.null(classification_service)) {
      classification_service <- find_classification_service(localId)
    }

  # Test if the classification_service given is among the supported
  # classification services. If not, give an error.
    if(!(classification_service %in% c("classifications", "correspondenceTables"))) {
      stop("Unknown classification service.")
    }

  # Test if argument content is among allowed contents url and data. If not,
  # give an error.
    if(!(content %in% c("url", "data"))) {
      stop("Argument 'content' has to be either 'data' or 'url'.")
    }

  # Change default content from data to url if no localId given.
  if(is.null(localId)) {
    content <- "url"
    # message("If localId not given, only urls accessed.")
  }

  # If localId provided, prepend with "/" to build url to the endpoint. If localId
  # is not provided the function accesses all the endpoints in the given classification
  # service. In this case, the url cannot end with '/'.
    if(!is.null(localId)) {
      localId <- paste0("/", localId)
    }

  # Create a vector that maps classification service name to url path.
    url_ends <- c("classifications" = "/classificationItems",
                 "correspondenceTables" = "/maps")

  # Set url.
    url <- paste0("https://data.stat.fi/api/classifications/v2/",
                  classification_service,
                  localId,
                  ifelse(content == "data", url_ends[classification_service], ""))

  # Access API.
    resp <- httr::GET(url, query = list(content = content, meta = "min", lang = lang))
    cont <- httr::content(resp, "text", encoding = "UTF-8")

  # Return.
    as.data.frame(jsonlite::fromJSON(cont))
}


#' Get url from API
#'
#' A wrapper for \code{access_API} function to get url.
#'
#' @param localId, character, localId of the required table.
#' @param classification_service character, \code{"correspondenceTable"} or
#'    \code{"classifications"}. Determines the classification service used. See Details for
#'    information on the classification services.
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
#' @param localId, character, local ID of the required correspondence table
#' @param lang, \code{"fi"}, \code{"en"}, or \code{"sv"}, language of the key required.
#'    Defaults to \code{"fi"}.
#' @param print_key_name, whether prints the long name of the correspondence table.
#'    Defaults to \code{TRUE}.
#'
#' @return data.frame, the key of the provided localId.
#' @export
#'
#' @examples
#'
#'    localId <- "kunta_1_20200101%23seutukunta_1_20200101"
#'    get_key(localId)
#'
get_key <- function(localId, lang = "fi", print_key_name = TRUE) {

  if(length(localId) > 1) {
    stop("Multiple localIds! This function currently gives you only one key at the time.")
  }

  key <- access_API(localId, content = "data", lang = lang,
                    classification_service = "correspondenceTables")
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
#' @param localId character, localId of the required correspondence table
#' @param lang, \code{"fi"}, \code{"en"}, or \code{"sv"}, language of the classification required.
#'    Defaults to \code{"fi"}.
#' @param print_series_name  whether prints the long name of the classification series.
#'    Defaults to \code{TRUE}.
#' @param as_named_vector, logical, whether to return the object as a named vector rather
#'    than data.frame. Defaults to \code{FALSE}.
#'
#' @return
#' @export
#'
#' @examples
#'
#'   localId <- "siviiliasiat_1_20140101"
#'   get_classification(localId)
#'
get_classification <- function(localId,
                               lang = "fi",
                               print_series_name = TRUE,
                               as_named_vector = FALSE) {

  if(length(localId) > 1) {
    stop("Multiple localIds! This function currently gives you only one series at the time.")
  }

  classif <- access_API(localId, content = "data", lang = lang,
                        classification_service = "classifications")
  text <- unlist(unique(classif$classification$classificationName))["name"]
  classif <- data.frame(code = classif$code,
                        name = unlist(lapply(classif$classificationItemNames, '[', "name")))

  output <- classif
  if(print_series_name) {message(text)}
  if(as_named_vector) {
    output <- classif$name
    names(output) <- classif$code
  }
  output
}


#' Get the year of the latest correspondence table
#'
#' @param offline, logical, whether works offline with package data. Defaults to TRUE.
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
    urls <- get_url(classification_service = "correspondenceTables")
    results <- urls_as_localId_df(urls)
    as.double(max(c(results$year1, results$year2)))
  }
}




#' Transform a list of urls into a data.frame that separates the relevant information of each localId.
#'
#' Used when searching localIds among endpoints. For internal use.
#'
#' @param urls, a list of characters, urls of localIds..
#'
#' @return data.frame containing the relevant information of each localId correponding to the input urls.
#' @import dplyr
#' @export
#'
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
    output <- cbind(results, nros) %>%
      dplyr::select(source, nro1, year1, date1, target, nro2, year2, date2)

  } else if(grepl("v2/classifications", urls)) {

    urls <- as.data.frame(sapply(urls, stringr::str_remove,
                                 paste0("https://data.stat.fi/api/classifications/v2/classifications/")))
    nro <- as.data.frame(matrix(unlist(lapply(urls, stringr::str_extract_all, "_\\d+_")), ncol = 1, byrow = TRUE))
    names(nro) <- "nro"
    results <- tidyr::separate(urls, url, c("series", "date1"), sep = "_\\d+_") %>%
      dplyr::mutate(year = substring(date1, 1,4),
                    date = substring(date1, 5,8))
    output <- cbind(results, nro) %>%
      dplyr::select(series, nro, year, date)
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

  indicator <- c(localId %in% search_classifications(as_localId = TRUE),
                 localId %in% search_keys(as_localId = TRUE))

  if(all(!indicator)) {stop("Classification service not found.")}

  c("classifications", "correspondenceTables")[indicator]

}


#' Create correspondence table localId
#'
#' Given the inputs, creates a localId. For internal use.
#'
#' @param source character
#' @param target character
#' @param year integer
#' @param year1 integer
#' @param year2 integer
#' @param date character defaults to "0101" which the most correspondence tables have.
#' @param date1 character
#' @param date2 character
#' @param nro1, character, defaults to "_1_".
#' @param nro2, character, defaults to "_1_".
#' @param input_vector named vector
#'
#' @return character
#' @export
#'
#' @examples
#'
#' # Create a localId for the key that maps "kunta" to "maakunta" for year 2015
#'    create_localId_name("kunta", "maakunta", year = 2015)
#'
create_localId_name <- function(source, target,
                                year, year1 = year, year2 = year,
                                date = "0101", date1 = date, date2 = date,
                                nro1 = "_1_", nro2 = "_1_",
                                input_vector = NULL) {

  if(!is.null(input_vector)) {
    source <- input_vector["source"]
    target <- input_vector["target"]
    date1 <- input_vector["date1"]
    date2 <- input_vector["date2"]
    year1 <- input_vector["year1"]
    year2 <- input_vector["year2"]
    nro1 <- input_vector["nro1"]
    nro2 <- input_vector["nro2"]
  }

  paste0(source, nro1, as.character(year1), date1,"%23", target, nro2, as.character(year2), date2)

}



# find_localId <- function(..., year = NULL, classification_service, localId_list = NULL) {
#
#   xs <- unlist(list(...))
#
#   if(classification_service == "correspondenceTables") {
#     if(is.null(localId_list)) {
#       results <- search_keys(xs[1], year = year, as_localId = TRUE)
#     } else {
#       results <- localId_list
#     }
#     for(x in xs) {
#       results <- grep(x, results, value = TRUE)
#     }
#   } else if(classification_service == "classifications") {
#
#     if(is.null(localId_list)) {
#       results <- search_classifications(xs[1], year = year, as_localId = TRUE)
#     } else {
#       results <- localId_list
#     }
#     for(x in xs) {
#       results <- grep(x, results, value = TRUE)
#     }
#   }
#   if(length(results) > 1) {stop("No unique localIds found! Maybe give more search words.")}
#   if(length(results) == 0) {stop("No localIds found!")}
#   results
# }

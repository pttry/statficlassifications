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
#' @param content  character, `"data"` or `"url"` determines the content of query
#' @param classification_service character, `"correspondenceTable"` or
#'    \code{"classifications"}. Determines the classification service used.
#' @param lang  `"fi"`, `"en"`, or `"sv"`, desired language.
#'    Defaults to `"fi"`.
#'
#' @return data.frame either the correspondence table or its url depending on argument `content`.
#' @export
#'
#' @examples
#'
#'   localId <- "kunta_1_20200101%23seutukunta_1_20200101"
#'   df <- access_API(localId, content = "data")
#'   access_API(localId, content = "url")
#'
access_API <- function(localId = NULL,
                       content = "data",
                       classification_service = NULL,
                       lang = "fi") {

  # Test if neither localId nor classification_service is given. If not, give
  # an error as the function does not know what to access.
    if(is.null(localId) & is.null(classification_service)) {
      stop("Neither a localId nor a classification_service provided!")
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
      message("If localId not given, only urls accessed.")
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
   # resp <- lapply(url, httr::GET, query = list(content = content, meta = "min", lang = lang))
    cont <- httr::content(resp, "text", encoding = "UTF-8")
   # cont <- lapply(resp, httr::content, as = "text", encoding = "UTF-8")


  # Return.
    as.data.frame(jsonlite::fromJSON(cont))
    # lapply(cont, jsonlite::fromJSON)
}


#' Get urls
#'
#' A wrapper for `access_API()` function to get url.
#'
#' @param localId character, localId of the required table.
#' @param classification_service character, `"correspondenceTable"` or
#'    `"classifications"`. Determines the classification service used.
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

  # Test if neither localId nor classification_service is given. If not, give
  # an error as the function does not know what to access.
  if(is.null(localId) & is.null(classification_service)) {
    stop("Please provide either a localId or a classification_service.")
  }

  # Access API.
  url <- access_API(localId, content = "url", classification_service = classification_service)

  # Return.
  names(url) <- "url"
  url
}


#' Get classification keys
#'
#' A wrapper for \code{access_API} to get classification keys.
#'
#' @param localId character, local ID of the required correspondence table
#' @param lang `"fi"`, `"en"`, or `"sv"`, desired language.
#'    Defaults to `"fi"`.
#'
#' @return data.frame, the key of the provided localId.
#' @export
#'
#' @examples
#'
#'    localId <- "kunta_1_20200101%23seutukunta_1_20200101"
#'    get_key(localId)
#'
get_key <- function(localId, lang = "fi") {

  if(length(localId) > 1) {
    return(lapply(localId, get_key, lang ))
  }

  # Access API.
  key <- access_API(localId, content = "data", lang = lang,
                    classification_service = "correspondenceTables")

  # Get metatext.
  text <- unique(key$correspondenceTable$correspondenceTableTexts)

  # Get codes and names.
  key <- data.frame(source_code = key$sourceItem$code,
                    source_name = unlist(lapply(key$sourceItem$classificationItemNames, '[', "name")),
                    target_code = key$targetItem$code,
                    target_name = unlist(lapply(key$targetItem$classificationItemNames, '[', "name")))


  # Return.
  message(text)
  key
}

#' Get classification series
#'
#' A wrapper for \code{access_API} to get classifications.
#'
#' @param localId character, localId of the required correspondence table
#' @param lang `"fi"`, `"en"`, or `"sv"`, desired language.
#'    Defaults to `"fi"`.
#' @param as_named_vector logical, whether to return the object as a named vector rather
#'    than data.frame. Defaults to \code{FALSE}.
#'
#' @return a data.frame, a classification series
#' @export
#'
#' @examples
#'
#'   localId <- "siviiliasiat_1_20140101"
#'   get_classification(localId)
#'
get_classification <- function(localId,
                               lang = "fi",
                               as_named_vector = FALSE) {

  if(length(localId) > 1) {
    return(lapply(localId, get_classification, lang, as_named_vector))
  }

  # Access API.
  classif <- access_API(localId, content = "data", lang = lang,
                        classification_service = "classifications")

  # Get metatext.
  text <- unlist(unique(classif$classification$classificationName))["name"]

  classif <- data.frame(code = classif$code,
                        name = unlist(lapply(classif$classificationItemNames, '[', "name")))

  # Format output
  output <- classif
  if(as_named_vector) {
    output <- classif$name
    names(output) <- classif$code
  }

  # Return
  message(text)
  output
}


#' Get the year of the latest correspondence table
#'
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
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

    suppressMessages(
       urls <- get_url(classification_service = "correspondenceTables")
    )
    results <- urls_as_localId_df(urls)
    as.double(max(c(results$year1, results$year2)))
  }
}




#' Transform a list of urls into a data.frame that separates the relevant information of each localId.
#'
#' Used when searching localIds among endpoints. For internal use.
#'
#' @param urls a list of characters, urls of localIds..
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
    results <- tidyr::separate(urls, url, c("source", "temp_var", "date2"), sep = "_\\d+_") |>
      tidyr::separate(temp_var, c("date1", "target"), sep = "#") |>
      dplyr::mutate(year1 = substring(date1, 1,4),
                    date1 = substring(date1, 5,8),
                    year2 = substring(date2, 1,4),
                    date2 = substring(date2, 5,8))
    output <- cbind(results, nros) |>
      dplyr::select(source, nro1, year1, date1, target, nro2, year2, date2)

  } else if(grepl("v2/classifications", urls)) {

    urls <- as.data.frame(sapply(urls, stringr::str_remove,
                                 paste0("https://data.stat.fi/api/classifications/v2/classifications/")))
    nro <- as.data.frame(matrix(unlist(lapply(urls, stringr::str_extract_all, "_\\d+_")), ncol = 1, byrow = TRUE))
    names(nro) <- "nro"
    results <- tidyr::separate(urls, url, c("series", "date1"), sep = "_\\d+_") |>
      dplyr::mutate(year = substring(date1, 1,4),
                    date = substring(date1, 5,8))
    output <- cbind(results, nro) |>
      dplyr::select(series, nro, year, date)
  }
  output
}


#' Finds a classification service for a localId
#'
#' Given a localId finds whether the it is a localId in classifications or correspondenceTables
#' classification services. For internal use.
#'
#' @param localId character
#'
#' @return classification service name(s), character vector
#' @export
#'
#' @examples
#'
#'     find_classification_service("siviiliasiat_1_20140101")
#'     find_classification_service("instit_sektori_1_19960101%23sektoriluokitus_1_20000101")
#'
find_classification_service <- function(localId) {

  if(length(localId) > 1) {
    return(vapply(localId, find_classification_service, character(1)))
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
#' @param nro1 character, defaults to "_1_".
#' @param nro2 character, defaults to "_1_".
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


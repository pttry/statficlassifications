#' API interface
#'
#' Given a local ID of a correspondence table fetches the table and reads it.
#'
#' @param localID character local ID of the required correspondence table
#'
#' @return data.frame
#' @export
#'
#' @examples
get_correspondencetable <- function(localID) {

  url <- paste0("https://data.stat.fi/api/classifications/v2/correspondenceTables/", localID, "/maps")
  as.data.frame(jsonlite::fromJSON(rawToChar(httr::GET(url,query = list(content = "data", meta = "min"))$content)))

}




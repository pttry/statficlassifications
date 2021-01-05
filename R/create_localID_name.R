#' Create correspondence table local ID
#'
#' Given the inputs creates the local ID of a correspondence table. For internal use.
#'
#' @param source character
#' @param target character
#' @param year integer
#' @param date character
#'
#' @return character
#' @export
#'
#' @examples
#'
create_localID_name <- function(source, target, year, date = "0101") {
  paste0(source, "_1_", as.character(year), date, "%23", target, "_1_", as.character(year), date)
}

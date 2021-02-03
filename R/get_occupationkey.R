#' Get an occupation name-code key
#'
#' @param code_level, disaggregation level
#' @param year, year of the key
#'
#' @return data.frame with occupation codes and names
#' @export
#'
#' @examples
#'
#'    get_occupationkey(1)
#'
get_occupationkey <- function(code_level = NULL, year = 2010) {

  localId <- search_series("ammatti", year = year, as_localId = TRUE)
  series <- get_series(localId, print_series_name = FALSE)

  if(!is.null(code_level)) {
    series <- dplyr::filter(series, nchar(code) == code_level)
  }

  series

}

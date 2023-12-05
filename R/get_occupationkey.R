#' Get an occupation name-code key
#'
#' @param code_level, disaggregation level
#' @param year, year of the key
#' @param lang, `"fi"`, `"sv"` or `"en"`. Language of the key.
#'    Defaults to `"fi"`.
#' @param as_named_vector, logical, whether to return a named vector rather than
#'    a data.frame. Defaults to `FALSE`.
#'
#' @return data.frame with occupation codes and names
#' @export
#'
#' @examples
#'
#'    get_occupationkey(1)
#'    get_occupationkey(1, as_named_vector = TRUE)
#'
get_occupationkey <- function(code_level = NULL, year = 2010, lang = "fi",
                              as_named_vector = FALSE) {

  localId <- search_classifications("ammatti", year = year, as_localId = TRUE)
  classif <- suppressMessages(get_classification(localId, lang = lang))

  if(!is.null(code_level)) {
    classif <- dplyr::filter(classif, nchar(code) == code_level)
  }
  output <- classif
  if(as_named_vector) {
    output <- classif$name
    names(output) <- classif$code
  }

  output

}

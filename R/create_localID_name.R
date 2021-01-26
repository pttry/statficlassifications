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

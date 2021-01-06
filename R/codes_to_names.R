#' Changes region codes to region names
#'
#' @param data data.frame
#' @param region character
#' @param year integer
#'
#' @return
#' @export
#'
#' @examples
#'
codes_to_names <- function(data, region, year) {

  if(!(region %in% c("kunta", "seutukunta", "maakunta", "suuralue"))) {
    stop("Argument 'region' has to be one of the following: 'kunta', 'seutukunta', 'maakunta' or 'suuralue")
  }

 recode(data, from = paste(region, "code", sep = "_"), to = paste(region, "name", sep = "_"), year = year)

}

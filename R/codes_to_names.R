#' Changes region codes to region names
#'
#' A wrapper that uses the statficlassifications::recode-function.
#'
#' @param data data.frame, the input data that contains a variable of region codes.
#' @param region character, the name of the variable of region codes.
#' @param year integer, the year of the applied classification key.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'     # Generate random municipal data
#'        data <- get_regionkey(year = 2020) %>%
#'                dplyr::select(kunta_code) %>%
#'                mutate(values = rnorm(n()))
#'
#'        codes_to_names(data, region = "kunta", year = 2020)
#'
codes_to_names <- function(data, region, year) {

  if(!(region %in% c("kunta", "seutukunta", "maakunta", "suuralue"))) {
    stop("Argument 'region' has to be one of the following: 'kunta', 'seutukunta', 'maakunta' or 'suuralue")
  }

 recode(data, from = paste(region, "code", sep = "_"), to = paste(region, "name", sep = "_"), year = year)

}

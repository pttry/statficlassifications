#' Change region names to region codes
#'
#' #' A wrapper that uses the statficlassifications::recode-function.
#'
#' @param data data.frame, the input data that contains a variable of region names.
#' @param region character, the name of the variable of region names.
#' @param year integer, the year of the applied classification key.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'#' # Replace municipality names by their codes
#'
#'     # Generate random municipal data
#'        data <- get_regionkey(year = 2020) %>%
#'                dplyr::select(kunta_name) %>%
#'                mutate(values = rnorm(n()))
#'
#'     names_to_codes(data, region = "kunta", year = 2020)
#'
names_to_codes <- function(data, region, year) {

  if(!(region %in% c("kunta", "seutukunta", "maakunta", "suuralue"))) {
    stop("Argument 'region' has to be one of the following: 'kunta', 'seutukunta', 'maakunta' or 'suuralue")
  }

  recode(data, from = paste(region, "name", sep = "_"), to = paste(region, "code", sep = "_"), year = year)

}

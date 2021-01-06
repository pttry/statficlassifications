#' Changes region names to region codes
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

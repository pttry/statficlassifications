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

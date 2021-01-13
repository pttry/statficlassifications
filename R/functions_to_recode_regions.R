#' Recode regional variables
#'
#' Recodes regional variables by e.g. changing kunnat to seutukunnat or changing names to codes
#' and vice versa. Uses the 'get_regionkey'-function.
#'
#' @param data data.frame, the input data that contains the regional variable.
#' @param from character, the regional variable name in the input data.
#' @param to character, the desired target in the classification key
#' @param year character or numeric, the year of the
#' @param leave logical, whether to leave the original 'from' variable in to the returing
#'    data.frame. Defaults to FALSE.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' # Replace municipality names by their codes
#'
#'     # Generate random municipal data
#'        data <- get_regionkey(year = 2020) %>%
#'                dplyr::select(kunta_name) %>%
#'                mutate(values = rnorm(n()))
#'     # Recode
#'     recode(data, "kunta_name", "kunta_code", year = 2020)
#'
#' # Add seutukunnat to municipal data
#'
#'     recode(data, "kunta_name", "seutukunta_name", year = 2020, leave = TRUE)
#'
recode <- function(data, from, to, year, leave = FALSE) {

  if(!(from %in% names(data))) {
    stop("input to argument 'from' not in the data!")
  }
  df <- dplyr::left_join(data,
                   dplyr::select(get_regionkey(year = year), from, to),
                   by = from)
  df <- dplyr::relocate(df, to)
  if(leave == FALSE) {
    df <- dplyr::select(df, -from)
  }
  df
}


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

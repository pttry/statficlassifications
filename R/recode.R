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

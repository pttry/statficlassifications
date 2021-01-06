#' Recode regional variables.
#'
#' @param data
#' @param from
#' @param to
#' @param year
#' @param leave
#'
#' @return
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
                   select(get_regionkey(year = year), from, to),
                   by = from)
  df <- dplyr::relocate(df, to)
  if(leave == FALSE) {
    df <- dplyr::select(df, -from)
  }
  df
}

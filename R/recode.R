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
recode <- function(data, from, to, year, leave = FALSE) {

  if(!(from %in% names(data))) {
    stop("input to argument 'from' not in the data!")
  }
  df <- dplyr::left_join(data,
                   select(get_regionkey(year = year), from, to),
                   by = from)
  df <- dplyr::select(df, to, everything())
  if(leave == FALSE) {
    df <- dplyr::select(df, -from)
  }
  df
}

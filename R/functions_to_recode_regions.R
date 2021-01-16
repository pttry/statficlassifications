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
recode_region <- function(data, from_orig, from, to, year = NULL, leave = FALSE) {

  regionkey <- dplyr::select(get_regionkey(year = year), to, from)
  regionkey <- dplyr::rename_with(regionkey, ~from_orig, from)
  df <- dplyr::left_join(data, regionkey, by = from_orig)

  if(leave == FALSE) {
    df <- dplyr::select(df, -from_orig)
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
names_to_codes <- function(data, from = NULL, year = NULL) {

  if(is.null(from)) {
    from <- detect_region_var(data)
  } else if(!(from %in% names(data))) {
    stop("input to argument 'from' not in the data!")
  } else {
    from <- rep(from, 2)
  }

  to <- paste(gsub("_.*", "", from[2]), "code", sep = "_")

  df <- recode(data, from_orig = from[1], from = from[2], to = to, year = year)
  df <- dplyr::relocate(df, to)
  df
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
codes_to_names <- function(data, from = NULL, year = NULL) {

  if(is.null(from)) {
    from <- detect_region_var(data)
  } else if(!(from %in% names(data))) {
    stop("input to argument 'from' not in the data!")
  } else {
    from <- rep(from, 2)
  }

  to <- paste(gsub("_.*", "", from[2]), "name", sep = "_")

  df <- recode(data, from_orig = from[1], from = from[2], to = to, year = year)
  df <- dplyr::relocate(df, to)
  df
}


#' Add regions to data
#'
#' Uses \code(get_regionkey)- and \code(recode)-functions to provide a fast and simple way to
#' add regions to data. Very pipe-friendly.
#'
#' @param data data.frame
#' @param from character, variable in the data that is used to add regions to the data. Defaults
#'    to NULL, in which case function tries to guess the variable in the data.
#' @param to character, the region to add.
#'
#' @return
#' @export
#'
#' @examples
#'
add_region <- function(data, to, from = NULL) {

  if(is.null(from)) {
    from <- detect_region_var(data)
  } else if(!(from %in% names(data))) {
    stop("input to argument 'from' not in the data!")
  } else {
    from <- rep(from, 2)
  }

  if(!(to %in% c("kunta", "seutukunta", "maakunta", "suuralue"))) {
    stop("Argument to has to be either 'kunta', 'seutukunta', 'maakunta' or 'suuralue'")
  }
  to <- paste(to, gsub(".*_", "", from[2]), sep = "_")

  recode(data, from_orig = from[1], from = from[2] , to = to, leave = TRUE)
}



#' Detect a region variable in data
#'
#' Given data, looks for the variable that contains regions. Returns the name of this variable
#' and the corresponding variable name in get_regionkey()
#'
#' @param data data.frame
#'
#' @return character(2) Returns the region variable in the original data and the correponding region
#'    varible in the get_regionkey()
#' @export
#'
#' @examples
#'
#'    # Generate random municipal data with random name for the regions
#'              data <- get_regionkey() %>%
#'                      dplyr::select(kunta_name) %>%
#'                      dplyr::rename_with(~paste(sample(letters, 4), collapse = "")) %>%
#'                      dplyr::mutate(values = rnorm(n()))
#'
#'              detect_region_var(data)
#'
detect_region_var <- function(data) {

  regionkey <- get_regionkey()
  for(var1 in names(data)) {
    for(var2 in names(regionkey)) {
      if(all(data[[var1]] %in% regionkey[[var2]])) {
        i <- var1
        j <- var2
        return(c(i,j))
      }
    }
  }


}

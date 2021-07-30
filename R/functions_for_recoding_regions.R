#' Recode classifications using key
#'
#' @param x vector of elements to be recoded
#' @param key the key to use in recoding
#' @param from source column in key for recoding
#' @param to target column in key for recoding
#'
#' @return vector or data.frame
#' @export
#'

statfi_recode <- function(x, key, from = NULL, to = NULL) {

  # Detecting from-column in key
    if(is.null(from)) {
      z <- sapply(names(key), function(name) {sum(x %in% key[[name]])})
      from <- names(key)[max(z) == z]
    }

  # Check if unique from-column in key found
    if(length(from) > 1) {stop("Source column in key not automatically detected. Use from-argument.")}

  # Check if all input vector elements in key from-column
    if(!all(x %in% key[[from]])) {message("Some of your inputs not in the key! Producing NAs.")}

  # If to not set, set as to all that are not from
    if(is.null(to)) {to <- names(key)[names(key) != from]}

  # Recode
    output <- key[to][match(x, key[[from]]),]

  # Check and warn if an element in input vector mapped to multiple element in key to-column
    if(length(output) > length(x)) {
      message("Mapping not unique!")
    }

  # Return
    output
  }


#' Recode regional variables
#'
#' Recodes regional variables by e.g. changing kunnat to seutukunnat or changing names to codes
#' and vice versa. Uses the 'get_regionkey'-function.
#'
#' @param x data.frame, the input vector that contains the regional variable.
#' @param to character, the desired target in the classification key
#' @param year character or numeric, the year of the
#' @param offline, logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return data.frame
#' @export
#'
#'

recode_region <- function(x, to = NULL, year = NULL, offline = TRUE) {

  from <- detect_region_level(x)
  year_in_data <- detect_region_year(x, from)

  if(is.null(year)) {
    year <- get_latest_year(offline = TRUE)
  }

  if(!(year %in% year_in_data)) {
    message(paste0("The region classification in data seems to fit to year(s) ",
                   paste(year_in_data, collapse = ", "),
                   ". A key corresponding to this year is used."))
  }

  regionkey <- get_regionkey(year = year, offline = offline)

  if(all(is_region_code(x))) {
    from <- paste0(from, "_code")
  } else if(all(is_region_name(x))) {
    from <- paste0(from, "_name")
  } else {
    stop("Input contains elements not region codes nor region names.")
  }

  statfi_recode(x, key = regionkey, from = from, to = to)
}

#' Add regions to data
#'
#' Uses get_regionkey- and recode-functions to provide a fast and simple way to
#' add regions to data. Very pipe-friendly.
#'
#' @param data data.frame
#' @param ... character(s), the region(s) to add.
#' @param from character, variable in the data that is used to add regions to the data. Defaults
#'    to NULL, in which case function tries to guess the variable in the data.
#' @param year, double, year of region classification
#' @param offline, logical, whether works offline with package data. Defaults to TRUE.
#'
#'
#' @return data.frame
#' @export
#'
#'
add_region <- function(data, ..., from = NULL, year = NULL, offline = TRUE) {

  to <- unlist(list(...))
  region_var <- detect_region_var(data, year = year, offline = offline)
  region_var_name_lgl <- sapply(region_var, grepl, pattern = "name")
  region_var_code_lgl <- sapply(region_var, grepl, pattern = "code")

  if(any(region_var_name_lgl)) {
  if(any(!is_region_name(data[[region_var[names(region_var)[region_var_name_lgl]]]]))) {
    stop("Unknown region names.")
  } }

  if(any(region_var_code_lgl)) {
  if(any(!is_region_code_with_prefix(data[[region_var[names(region_var)[region_var_code_lgl]]]]))) {
    stop("Standardize your region codes first.")
  }  }

  from_key <- names(region_var)

  if(is.null(from)) {
    from <- region_var
    names(from) <- NULL
  } else if(!(from %in% names(data))) {
    stop("input to argument 'from' not in the data!")
  }

  if(!all(to %in% c("kunta", "seutukunta", "maakunta", "suuralue"))) {
    stop("Argument to has to be either 'kunta', 'seutukunta', 'maakunta' or 'suuralue'")
  }
  to <- paste(to, gsub(".*_", "", from_key), sep = "_")

  new_var <- recode_region(data[[from]], year = year, to = to)
  new_names <- c(names(data), to)
  data <- cbind(data, data.frame(new_var))
  names(data) <- new_names
  data
}

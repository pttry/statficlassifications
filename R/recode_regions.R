#' Recode regional variables
#'
#' Recodes regional variables by e.g. changing kunnat to seutukunnat or changing names to codes
#' and vice versa. A wrapper for statfi_recode function that uses the 'get_regionkey'-function
#' to produce the key.
#'
#' @param x data.frame, the input vector that contains the regional variable.
#' @param to character, the desired target in the classification key
#' @param from character, the original classification key
#' @param year character or numeric, the year of the
#' @param offline, logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return data.frame
#' @export
#'
#'

recode_region <- function(x, to, from = NULL, year = NULL, offline = TRUE) {

  if (is.null(from)){
    from <- detect_region_level(x)
    if (length(from) > 1){
      message("Detected level could be one of following: ", from, ". The first one is used")
      from <- from[1]
    }
  }
  year_in_data <- detect_region_year(x, from)

  if(is.null(year)) {
    year <- get_latest_year(offline = TRUE)
  }

  if(!(year %in% year_in_data)) {
    message(paste0("The region classification in data seems to fit year(s) ",
                   paste(year_in_data, collapse = ", "),
                   ". A key corresponding to this year is used."))
  }

  regionkey <- get_regionkey(gsub("_.*", "", from),
                             gsub("_.*", "", to),
                             year = year,
                             offline = offline)

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

  if(any(!(grepl("code", to) | grepl("name", to)))) {
    to <- c(to, paste(to[!(grepl("code", to) | grepl("name", to))], c("code", "name"), sep = "_"))
    to <- to[grepl("code", to) | grepl("name", to)]
  }

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

  if(is.null(from)) {
    from <- region_var
    names(from) <- NULL
  } else if(!(from %in% names(data))) {
    stop("input to argument 'from' not in the data!")
  }

  new_var <- recode_region(x = data[[from[1]]], to = to, year = year, offline = offline)
  new_names <- c(names(data), to)
  data <- cbind(data, data.frame(new_var))
  names(data) <- new_names
  data
}

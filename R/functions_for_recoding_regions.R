#' Title
#'
#' @param x
#' @param year
#' @param key
#' @param leave
#'
#' @return
#' @export
#'
#' @examples
statfi_recode <- function(x, ..., year = NULL, key = NULL, leave = FALSE) {

  searchwords <- unlist(list(...))

  if(is.null(key)) {
    localId <- search_classifications(searchwords, year = year, as_localId = TRUE)
    key <- get_classification(localId)
  }

  logical <- sapply(names(key), function(var_name) {all(x %in% key[[var_name]])})

  if(all(!logical)) {stop("Some of your inputs not in the key!")}

  from <- names(key)[logical]
  x_df <- data.frame(x)
  names(x_df) <- from
  output <- dplyr::left_join(x_df, key, by = from)

  if(!leave) {
    output <- output[,names(key)[!logical]]
  }
  if(length(output) > length(x)) {
    warning("Mapping not unique!")
  }

  output
}




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
#'        data <- get_regionkey("kunta") %>%
#'                dplyr::select(kunta_name) %>%
#'                dplyr::mutate(values = rnorm(dplyr::n()))
#'     # Recode
#'     recode_region(data, "kunta_name", "kunta_name", "kunta_code")
#'
#' # Add seutukunnat to municipal data
#'
#'     recode_region(data, "kunta_name", "kunta_name", "seutukunta_name", year = 2020, leave = TRUE)
#'
recode_region <- function(data, from_orig, from, to, year = NULL, leave = FALSE, offline = TRUE) {

  year_in_data <- detect_region_year(data[[from_orig]], detect_region_level(data[[from_orig]]))

  if(is.null(year)) {
    year <- get_latest_year(offline = TRUE)
  }

  if(!(year %in% year_in_data)) {
    message(paste0("The region classification in data seems to fit to year(s) ",
                   paste(year_in_data, collapse = ", "),
                  ". A key corresponding to this year is used."))
  }

  regionkey <- get_regionkey(year = year, offline = offline)

  regionkey <- dplyr::select(regionkey, to, from)
  regionkey <- dplyr::rename_with(regionkey, ~from_orig, from)
  df <- dplyr::left_join(data, regionkey, by = from_orig)

  if(leave == FALSE) {
    df <- dplyr::select(df, -from_orig)
    df <- dplyr::relocate(df, to)
  }
  df[!duplicated(df),]
}




#' Add regions to data
#'
#' Uses get_regionkey- and recode-functions to provide a fast and simple way to
#' add regions to data. Very pipe-friendly.
#'
#' @param data data.frame
#' @param to character, the region to add.
#' @param from character, variable in the data that is used to add regions to the data. Defaults
#'    to NULL, in which case function tries to guess the variable in the data.
#' @param year, double, year of region classification
#' @param offline, logical, whether works offline with package data. Defaults to TRUE.
#'
#'
#' @return
#' @export
#'
#' @examples
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

  recode_region(data, from_orig = from, from = from_key, year = year, to = to, leave = TRUE)
}


#add_region <- function(data, to, from = NULL, year = NULL, offline = TRUE) {

# if(is.null(from)) {
#   from <- detect_region_var(data, year = year, offline = offline)
# } else if(!(from %in% names(data))) {
#   stop("input to argument 'from' not in the data!")
# } else {
#   from <- list(name_orig = from, name_key = names(from))
# }
#
# if(!(to %in% c("kunta", "seutukunta", "maakunta", "suuralue"))) {
#   stop("Argument to has to be either 'kunta', 'seutukunta', 'maakunta' or 'suuralue'")
# }
# to <- paste(to, gsub(".*_", "", from$name_key), sep = "_")
#
# recode_region(data, from_orig = from$name_orig, from = from$name_key, to = to, leave = TRUE)
# }
# detect_region_var <- function(data, offline = FALSE) {
#
#   regionkey <- get_regionkey(offline = offline)
#
#   i <- 1
#   j <- 1
#   name_orig <- numeric()
#   name_key <- numeric()
#   for(var_orig in names(data)) {
#     for(var_key in names(regionkey)) {
#       if(all(data[[var_orig]] %in% regionkey[[var_key]])) {
#         name_orig[i] <- var_orig
#         name_key[j] <- var_key
#         i <- i + 1
#         j <- j + 1
#       }
#     }
#   }
#   if(length(name_orig) == 0 | length(name_key) == 0) {
#     stop("Region variable not automatically detected!")
#   }
#   return(list(name_orig = name_orig, name_key = name_key))
#
# }




#' Detect region variables in data
#'
#' Given data, looks for the variable that contains regions. Returns the name of this variable
#' and the corresponding variable name in get_regionkey()
#'
#' @param data data.frame
#' @param year, double, year of the used classification
#' @param offline, logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return character(2) Returns the region variable in the original data and the corresponding region
#'    variable in the get_regionkey()
#' @export
#'
#' @examples
#'
#'    # Generate random municipal data with random name for the regions
#'              data <- get_regionkey() %>%
#'                      dplyr::select(kunta_name) %>%
#'                      dplyr::rename_with(~paste(sample(letters, 4), collapse = "")) %>%
#'                      dplyr::mutate(values = rnorm(dplyr::n()))
#'
#'              detect_region_var(data)
#'
detect_region_var <- function(data, year = NULL, offline = TRUE) {

  region_name_var <- purrr::map_lgl(names(data), function(x) {all(is_region_name(x = data[[x]], year = year, offline = offline,
                                                                                 allow_nonstandard_names = TRUE))})
  region_code_var <- purrr::map_lgl(names(data), function(x) {all(is_region_code(data[[x]], year = year, offline = offline))})

  if(all(!(c(region_name_var, region_code_var)))) {
    stop("Region variable not automatically detected!")
  }

  region_name_var <- names(data)[region_name_var]
  region_code_var <- names(data)[region_code_var]

  region_name_var_level <- unlist(sapply(region_name_var, function(x) detect_region_level_name(data[[x]])))
  region_code_var_level <- unlist(sapply(region_code_var, function(x) detect_region_level_code(data[[x]])))

  if(length(region_code_var_level) == 0 & length(region_code_var) > 0) {
    region_code_var_level <- "alue_code"
  } else if(length(region_code_var) > 0) {
    region_code_var_level <- paste(region_code_var_level, "code", sep = "_")
  }

  if(length(region_name_var_level) == 0 & length(region_name_var) > 0) {
    region_name_var_level <- "alue_name"
  } else if(length(region_name_var) > 0) {
    region_name_var_level <- paste(region_name_var_level, "name", sep = "_")
  }

  output <- c(region_name_var, region_code_var)
  names(output) <- c(region_name_var_level, region_code_var_level)
  output
}

detect_region_level_code <- function(x, year = NULL, offline = TRUE) {

  logical <- purrr::map_lgl(prefix_name_key[-1,]$name, function(region_level)
  {all(is_region_code(x, region_level, year = year, offline = offline))})
  prefix_name_key[-1,]$name[logical]

}

detect_region_level_name <- function(x, year = NULL, offline = TRUE) {

  logical <- purrr::map_lgl(prefix_name_key[-1,]$name, function(region_level)
  {all(is_region_name(x, region_level, year = year, offline = offline, allow_nonstandard_names = TRUE))})
  prefix_name_key[-1,]$name[logical]

}

detect_region_level <- function(x, year = NULL, offline = TRUE) {
  c(detect_region_level_code(x, year = year, offline = offline),
    detect_region_level_name(x, year = year, offline = offline))
}

#' Detect year of region classification
#'
#' Detects year of classification from the number of unique regions using the idea that
#' the number of regions vary yearly. Given a vector of region names or codes and region
#' level determines the year of regional classification. Returns all matching years.
#'
#' @param x
#' @param region_level
#'
#' @return vector
#' @export
#'
#' @examples
#'    df <- get_regionclassification("kunta", year = 2010)
#'    detect_region_year(df$kunta_name, region_level = "kunta")
#'
detect_region_year <- function(x, region_level) {

  statficlassifications::number_of_regions %>%
    dplyr::filter(region_level == region_level,
                  number_of_regions == length(unique(x))) %>%
    `$`(year)

}

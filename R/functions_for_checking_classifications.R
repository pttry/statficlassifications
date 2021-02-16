#' Check region codes
#'
#' @param x character vector or factor
#' @param year double or character, year of classification
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return
#' @export
#'
#'
check_region_codes <- function(x, year = NULL, offline = TRUE) {

  if(is.vector(x)){
    x <- check_region_codes_vct(x, offline = offline, year = year)
  } else if(is.factor(x)) {
    x <- check_region_codes_fct(x, offline = offline, year = year)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}

#' @describeIn check_region_codes
#'
#' Check region codes of a vector
#'
#' For internal use.
#'
#' @export
#'
check_region_codes_vct <- function(x, year = NULL, offline = TRUE) {

  logical <- is_region_code_with_prefix(x, offline = offline, year = year)
  if(all(logical)) {
    return(TRUE)
  } else {
    return(paste0("Region code(s) ",
                  paste(x[!logical], collapse = ", "),
                  " not recognized as region code(s)",
                  ifelse(!is.null(year), paste0("for year ", year, "."), ".")))
  }
}

#' @describeIn check_region_codes
#'
#' Check region codes of a vector
#'
#' For internal use.
#'
#' @export
#'
check_region_codes_fct <- function(x, year = NULL, offline = TRUE) {

  check_region_codes_vct(levels(x), offline = offline, year = year)

}

#' Check region names
#'
#' @param x character vector or factor
#' @param year double or character, year of classification
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return
#' @export
#'
#'
check_region_names <- function(x, year = NULL, offline = TRUE) {

  if(is.vector(x)){
    x <- check_region_names_vct(x, offline = offline, year = year)
  } else if(is.factor(x)) {
    x <- check_region_names_fct(x, offline = offline, year = year)
  } else {
    stop("Argument not a vector or factor.")
  }
  x
}

#' @describeIn check_region_names
#'
#' Check region names of a vector
#'
#' For internal use.
#'
#' @export
#'
check_region_names_vct <- function(x, year = NULL, offline = TRUE) {

  logical <- is_region_name(x, offline = offline, year = year)
  if(all(logical)) {
    return(TRUE)
  } else {
    return(paste0("Region name(s) ",
                  paste(x[!logical], collapse = ", "),
                  " not recognized as region name(s)",
                  ifelse(!is.null(year), paste0("for year ", year, "."), ".")))
  }
}

#' @describeIn check_region_names
#'
#' Check region names of a factor
#'
#' For internal use.
#'
#' @export
#'
check_region_names_fct <- function(x, year = NULL, offline = TRUE) {
  check_region_names_vct(levels(x), offline = offline, year = year)
}

#' Check if region names and codes correspond as in regionkey.
#'
#' @param data data.frame
#' @param region_name_var character the name of the regional name variable in the data
#' @param region_code_var character the name of the regional code variable in the data
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return
#' @export
#'
check_region_var_name_code_correspondence <- function(data,
                                                      region_name_var, region_code_var,
                                                      offline = TRUE) {

  regions <- c("kunta", "seutukunta", "maakunta", "suuralue", "ely")

  regionkey <- get_regionkey(offline = offline)
  regionkey <- purrr::map(regions, ~tidyr::unite(regionkey, !!.x, paste(.x, c("name", "code"), sep = "_"))) %>%
    purrr::flatten() %>%
    as.data.frame() %>%
    dplyr::select(regions)

  data <- tidyr::unite(data, region_var, region_name_var, region_code_var)

  logical <- logical(length(names(regionkey)))
  names(logical) <- names(regionkey)
  for(var in names(regionkey)) {
    if(all(data$region_var %in% regionkey[[var]])) {
      logical[var] <- TRUE
    }
  }
  any(logical)
}


#' Check if character is prefixed region code
#'
#' @param x character
#' @param region_level character, optional region level of the input region codes
#' @param year double
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#'
#' @return
#' @export
#'
#' @examples
#'
#'   is_region_code(c("KU005", "005"))
#'   is_region_code_with_prefix(c("KU005", "005"))
#'   is_region_code_without_prefix(c("KU005", "005"))
#'   is_region_code_with_prefix(c("KU005", "005"), region_level = "maakunta")
#'   is_region_code_without_prefix(c("KU005", "005"), region_level = "maakunta")
#'
is_region_code <- function(x, region_level = NULL, year = NULL, offline = TRUE) {
  is_region_code_with_prefix(x, region_level = region_level, year = year, offline = offline) |
    is_region_code_without_prefix(x, region_level = region_level, year = year, offline = offline)
}

#' @describeIn is_region_code
#'
#' Check if input is region code with prefix.
#'
#'
#' @export
#'
is_region_code_with_prefix <- function(x, region_level = NULL, year = NULL, offline = TRUE) {

  codes <- get_regionclassification(region_level, year = year, offline = offline, suppress_message = TRUE, only_codes = TRUE)
  as.vector(x) %in% codes

}

#' @describeIn is_region_code
#'
#' Check if input is region code without prefix.
#'
#'
#' @export
#'
is_region_code_without_prefix <- function(x, region_level = NULL, year = NULL, offline = TRUE) {

  codes <- get_regionclassification(region_level, year = year, offline = offline, suppress_message = TRUE, only_codes = TRUE)
  codes <- as.double(sapply(codes, gsub, pattern = "[^0-9.-]", replacement = ""))
  suppressWarnings(as.double(as.vector(x)) %in% codes)

}

#' Check if character is region name.
#'
#' @param x character
#' @param region_level character, optional region level of the input region codes
#' @param year double
#' @param offline logical, whether works offline with package data. Defaults to TRUE.
#' @param allow_nonstandard_names logical, whether to accept a broader set of names as
#'    region names.
#'
#' @return
#' @export
#'
#' @examples
#'
#'  is_region_name(c("Kainuu", "Kainuun maakunta"))
#'  is_region_name(c("Kainuu", "Kainuun maakunta"), allow_nonstandard_names = TRUE)
#'
is_region_name <- function(x, region_level = NULL, year = NULL, offline = TRUE, allow_nonstandard_names = FALSE) {

  if(allow_nonstandard_names) {
    names <- statficlassifications::region_name_to_code_key %>%
      dplyr::filter(grepl(paste(name_to_prefix(region_level), collapse = "|"), alue_code))
    names <- names$alue_name
  } else {
    names <- get_regionclassification(region_level, year = year, offline = offline, suppress_message = TRUE, only_names = TRUE)
  }
  x %in% names

}

